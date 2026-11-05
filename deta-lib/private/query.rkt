#lang racket/base

(require db
         (only-in racket/class send)
         racket/contract/base
         racket/match
         racket/struct
         (prefix-in ast: "ast.rkt")
         "connection.rkt"
         "dialect/dialect.rkt"
         "dialect/postgresql.rkt"
         "field.rkt"
         "schema.rkt")

;; struct ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-empty-query
 (struct-out opts)
 (struct-out query)
 emit-query)

(struct opts (project-virtual-fields?)
  #:transparent)

;; schema: (or/c #f schema?)
;; sources: (hash/c string? (or/c #f schema?))
(struct query (schema sources opts stmt)
  #:transparent
  #:property prop:statement
  (lambda (self c)
    (define dialect (connection-dialect c))
    (define-values (query args)
      (emit-query self dialect))

    ;; We grab the base connection s.t. querying `virtual-connection's
    ;; works. `virtual-statement' does the same thing so we should be
    ;; fine, compatibility-wise, but this behavior isn't specified
    ;; anywhere so it may break on us w/o notice.
    (define prepared
      (prepare (or (send c get-base) c) query))

    (cond
      [(null? args) prepared]
      [else (bind-prepared-statement prepared (dialect-prepare-parameters dialect prepared args))]))

  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (_) 'query)
   (lambda (self)
     (define-values (query args)
       (emit-query self postgresql-dialect))
     (cons query args))))

(define (make-empty-query)
  (make-query
   (ast:make-select)))

(define (make-query
         #:schema [schema #f]
         #:sources [sources (hash)]
         #:project-virtual-fields? [project-virtual? #f]
         stmt)
  (query
   #;schema schema
   #;sources sources
   #;opts (opts project-virtual?)
   #;stmt stmt))

(define (emit-query q dialect)
  (dialect-emit-query dialect (finalize q)))

;; combinators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [delete (-> select-query? query?)]
  [from (-> any/c #:as symbol? query?)]
  [group-by (-> select-query? ast:expr? ast:expr? ... query?)]
  [join
   (-> select-query?
       #:type (or/c 'inner 'left 'right 'full 'cross)
       #:lateral? boolean?
       #:with (or/c ast:subquery? schema? string? symbol?)
       #:as symbol?
       #:on (or/c #f ast:expr?)
       query?)]
  [limit (-> query? (or/c ast:scalar? ast:placeholder?) query?)]
  [offset (-> query? (or/c ast:scalar? ast:placeholder?) query?)]
  [or-where (-> query? ast:expr? query?)]
  [order-by (-> select-query? ordering/c ordering/c ... query?)]
  [project-onto (-> query? schema? query?)]
  [project-virtual-fields (-> query? query?)]
  [returning (-> query? ast:expr? ast:expr? ... query?)]
  [select
   (->* [query? ast:expr?]
        [#:distinct? (or/c boolean? list?)]
        #:rest (listof ast:expr?)
        query?)]
  [select-for-schema
   (-> query?
       (or/c schema? symbol?)
       string?
       (hash/c symbol? ast:expr?)
       query?)]
  [select-for-update
   (-> query? query?)]
  [subquery (-> select-query? ast:subquery?)]
  [union (-> select-query? select-query? query?)]
  [update (-> select-query? assignment/c assignment/c ... query?)]
  [where (-> query? ast:expr? query?)]))

(define (delete q)
  (define delete-stmt
    (match (query-stmt q)
      [(struct*
        ast:select
        ([from (ast:from (list table) _)]
         [where where]))
       (when (contains-subquery? table)
         (raise-argument-error 'delete "a real table to delete from" table))
       (ast:make-delete
        #:from (ast:make-from #:tables (list table))
        #:where where)]))
  (struct-copy
   query q
   [stmt delete-stmt]))

(define (from source #:as alias)
  (define alias-str
    (symbol->string alias))
  (cond
    [(string? source)
     (make-query
      #:sources (hash alias-str #f)
      (ast:make-select
       #:from (ast:make-from
               #:tables (list (ast:as (ast:table source) alias-str)))))]

    [(symbol? source)
     (define schema
       (schema-registry-lookup source))
     (make-query
      #:schema schema
      #:sources (hash alias-str schema)
      (ast:make-select
       #:from (ast:make-from
               #:tables (list (ast:as (ast:table (schema-table schema)) alias-str)))
       #:columns (for/list ([f (in-list (schema-fields/nonvirtual schema))])
                   (ast:column (ast:qualified alias-str (field-name f))))))]

    [(ast:subquery? source)
     (make-query
      #:sources (hash alias-str #f)
      (ast:make-select
       #:from (ast:make-from #:tables (list (ast:as source alias-str)))))]

    [else
     (raise-argument-error 'from "a table name, a schema name or a subquery" source)]))

(define (join q
              #:type type
              #:lateral? lateral?
              #:with tbl-e
              #:as alias
              #:on constraint)
  (define maybe-schema
    (cond
      [(schema? tbl-e) tbl-e]
      [(symbol? tbl-e) (schema-registry-lookup tbl-e)]
      [else #f]))
  (define tbl-clause
    (cond
      [maybe-schema (ast:table (schema-table maybe-schema))]
      [(string? tbl-e) (ast:table tbl-e)]
      [else tbl-e]))
  (define sources
    (hash-set (query-sources q) (symbol->string alias) maybe-schema))
  (define stmt
    (query-stmt q))
  (define the-join
    (ast:join
     type lateral?
     (ast:as tbl-clause alias)
     (maybe-resolve sources constraint)))
  (define the-other-joins
    (ast:from-joins (ast:select-from stmt)))
  (define updated-from
    (struct-copy
     ast:from (ast:select-from stmt)
     [joins (append the-other-joins (list the-join))]))
  (define updated-stmt
    (struct-copy
     ast:select stmt
     [from updated-from]))
  (struct-copy
   query q
   [sources sources]
   [stmt updated-stmt]))

(define (select q column0
                #:distinct? [distinct? #f]
                . columns)
  (define sources
    (query-sources q))
  (define select-stmt
    (struct-copy
     ast:select (query-stmt q)
     [distinct? (if (pair? distinct?)
                    (resolve-all sources distinct?)
                    distinct?)]
     [columns (resolve-all sources (cons column0 columns))]))
  (struct-copy
   query q
   [schema #f]
   [stmt select-stmt]))

(define (select-for-schema q schema-or-id tbl-alias customizations)
  (define s
    (schema-registry-lookup schema-or-id))
  (define columns
    (for/list ([fld (schema-fields s)])
      (hash-ref
       #;ht customizations
       #;key (field-id fld)
       #;failure-result
       (lambda ()
         (ast:qualified tbl-alias (field-name fld))))))
  (define sources
    (query-sources q))
  (define q-with-sources
    (cond
      [(hash-ref sources tbl-alias #f) q]
      [else (struct-copy query q [sources (hash-set sources tbl-alias s)])]))
  (define updated-q
    (apply select q-with-sources columns))
  (project-onto updated-q s))

(define (select-for-update q)
  (define select-for-update-stmt
    (match (query-stmt q)
      [(? ast:select? stmt)
       (struct-copy
        ast:select stmt
        [for-spec (ast:select-for 'update)])]
      [_
       (raise-argument-error
        'select-for-update
        "a SELECT query" q)]))
  (struct-copy
   query q
   [stmt select-for-update-stmt]))

(define (limit q n)
  (define limit-stmt
    (struct-copy
     ast:select (query-stmt q)
     [limit (ast:limit n)]))
  (struct-copy
   query q
   [stmt limit-stmt]))

(define (group-by q column0 . columns)
  (define all-columns
    (resolve-all
     (query-sources q)
     (cons column0 columns)))
  (define group-by-stmt
    (match (query-stmt q)
      [(and (struct* ast:select ([group-by #f])) stmt)
       (struct-copy ast:select stmt [group-by (ast:group-by all-columns)])]
      [(and (struct* ast:select ([group-by (ast:group-by existing-columns)])) stmt)
       (struct-copy ast:select stmt [group-by (ast:group-by (append existing-columns all-columns))])]))
  (struct-copy
   query q
   [stmt group-by-stmt]))

(define (offset q n)
  (define offset-stmt
    (struct-copy
     ast:select (query-stmt q)
     [offset (ast:offset n)]))
  (struct-copy
   query q
   [stmt offset-stmt]))

(define ordering/c
  (list/c ast:expr? (or/c 'asc 'desc) (or/c #f 'nulls-first 'nulls-last)))

(define (union q1 q2)
  (define (union* s1 s2)
    (match s1
      [(struct* ast:select ([union #f]))
       (struct-copy ast:select s1 [union (ast:union s2)])]
      [(struct* ast:select ([union u]))
       (struct-copy ast:select s1 [union (ast:union (union* (ast:union-stmt u) s2))])]))
  (define union-stmt
    (match (query-stmt q1)
      [(and (struct* ast:select ([union #f])) stmt)
       (struct-copy
        ast:select stmt
        [union (ast:union (resolve-query q2))])]
      [(and (struct* ast:select ([union u])) stmt)
       (struct-copy
        ast:select stmt
        [union (ast:union (union* (ast:union-stmt u) (resolve-query q2)))])]))
  (struct-copy
   query q1
   [stmt union-stmt]))

(define (order-by q ordering0 . orderings)
  (define all-orderings
    (resolve-orderings
     (query-sources q)
     (cons ordering0 orderings)))
  (define order-by-stmt
    (match (query-stmt q)
      [(and (struct* ast:select ([order-by #f])) stmt)
       (struct-copy
        ast:select stmt
        [order-by (ast:order-by all-orderings)])]
      [(and (struct* ast:select ([order-by (ast:order-by existing-orderings)])) stmt)
       (struct-copy
        ast:select stmt
        [order-by (ast:order-by (append existing-orderings all-orderings))])]))
  (struct-copy
   query q
   [stmt order-by-stmt]))

(define (project-onto q s)
  (struct-copy query q [schema s]))

(define (project-virtual-fields q)
  (struct-copy query q [opts (opts #t)]))

(define (returning q e0 . es)
  (define all-exprs
    (resolve-all
     (query-sources q)
     (cons e0 es)))
  (define (append-exprs e)
    (match e
      [#f
       (ast:returning all-exprs)]
      [(ast:returning existing-exprs)
       (ast:returning (append existing-exprs all-exprs))]))
  (define returning-stmt
    (match (query-stmt q)
      [(and (? ast:insert?) stmt)
       (struct-copy
        ast:insert stmt
        [returning (append-exprs (ast:insert-returning stmt))])]
      [(and (? ast:update?) stmt)
       (struct-copy
        ast:update stmt
        [returning (append-exprs (ast:update-returning stmt))])]
      [(and (? ast:delete?) stmt)
       (struct-copy
        ast:delete stmt
        [returning (append-exprs (ast:delete-returning stmt))])]))
  (struct-copy
   query q
   [stmt returning-stmt]))

(define (subquery q)
  (ast:subquery
   (resolve-query q)))

(define (update q assignment0 . assignments)
  (define sources
    (query-sources q))
  (define update-stmt
    (match (query-stmt q)
      [(struct*
        ast:select
        ([from (ast:from (list table) _)]
         [where where]))
       (when (contains-subquery? table)
         (raise-argument-error 'update "a table to update" table))
       ;; Assignment columns are unqualified, so they are resolved
       ;; against the table being updated rather than against every
       ;; source in the query.
       (define alias
         (match table
           [(ast:as _ alias) alias]
           [_ #f]))
       (define all-assignments
         (for/list ([a (in-list (cons assignment0 assignments))])
           (cons
            (resolve-assignment-column sources alias (car a))
            (resolve sources (cdr a)))))
       (ast:make-update
        #:table table
        #:assignments (ast:assignments all-assignments)
        #:where where)]))
  (struct-copy
   query q
   [stmt update-stmt]))

(define (where q e)
  (add-where q e 'and))

(define (or-where q e)
  (add-where q e 'or))


;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (select-query? q)
  (and (query? q)
       (ast:select?
        (query-stmt q))))

(define (contains-subquery? f)
  (match f
    [(ast:subquery _)             #t]
    [(ast:as (? ast:subquery?) _) #t]
    [_                            #f]))

(define assignment/c
  (cons/c ast:column? ast:expr?))

(define (add-where q e0 op)
  (define e
    (resolve (query-sources q) e0))
  (define where-stmt
    (match (query-stmt q)
      [(and (struct* ast:select ([where #f])) stmt)
       (struct-copy ast:select stmt [where (ast:where e)])]
      [(and (struct* ast:update ([where #f])) stmt)
       (struct-copy ast:update stmt [where (ast:where e)])]
      [(and (struct* ast:delete ([where #f])) stmt)
       (struct-copy ast:delete stmt [where (ast:where e)])]
      [(and (struct* ast:select ([where (ast:where existing-e)])) stmt)
       (struct-copy ast:select stmt [where (ast:where (ast:app (ast:ident op) (list existing-e e)))])]
      [(and (struct* ast:update ([where (ast:where existing-e)])) stmt)
       (struct-copy ast:update stmt [where (ast:where (ast:app (ast:ident op) (list existing-e e)))])]
      [(and (struct* ast:delete ([where (ast:where existing-e)])) stmt)
       (struct-copy ast:delete stmt [where (ast:where (ast:app (ast:ident op) (list existing-e e)))])]))
  (struct-copy
   query q
   [stmt where-stmt]))


;; resolve ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The query macros leave field names as symbols in the tree and the job
;; of the resolver is to map qualified fields, and unqualified fields in
;; an update clause, back to their canonical column #:names. When the
;; resolver meets a symbol column id, it tries to resolve it back to
;; its canonical name. If it has a #f source schema for a given alias,
;; then it always resolves the column by converting it to a string using
;; id->column-name. If it has an actual schema for a given alias, then
;; it resolves the column name by looking up that field in the schema.
;; If it doesn't have a mapping for a given alias, it punts until the
;; next resolve call or until it is called with finalize? set to #t,
;; in which case any remaining unresolvable columns are converted to a
;; string via id->column-name.
;;
;; The combinators each resolve whatever they introduce. When emitted, a
;; query is finalized, performing a final resolution over the tree. This
;; way, the number of resolve passes remains linear.

(define (finalize q)
  (resolve
   (query-sources q)
   (query-stmt q)
   #;finalize? #t))

(define (resolve sources e [finalize? #f])
  (match e
    ;; statements
    [(ast:delete from where returning)
     (ast:delete
      (resolve sources from finalize?)
      (maybe-resolve sources where finalize?)
      (maybe-resolve sources returning finalize?))]
    [(ast:insert into columns column-values returning)
     (ast:insert
      (resolve sources into finalize?)
      (resolve-all sources columns finalize?)
      (resolve-all sources column-values finalize?)
      (maybe-resolve sources returning finalize?))]
    [(ast:update table assignments where returning)
     (ast:update
      (resolve sources table finalize?)
      (resolve sources assignments finalize?)
      (maybe-resolve sources where finalize?)
      (maybe-resolve sources returning finalize?))]
    [(ast:select distinct? columns from where group-by union order-by limit offset for-spec)
     (ast:select
      (if (pair? distinct?)
          (resolve-all sources distinct? finalize?)
          distinct?)
      (resolve-all sources columns finalize?)
      (maybe-resolve sources from finalize?)
      (maybe-resolve sources where finalize?)
      (maybe-resolve sources group-by finalize?)
      (maybe-resolve sources union finalize?)
      (maybe-resolve sources order-by finalize?)
      limit
      offset
      for-spec)]

    ;; clauses
    [(ast:assignments pairs)
     (ast:assignments (resolve-pairs sources pairs finalize?))]
    [(ast:from tables joins)
     (ast:from
      (resolve-all sources tables finalize?)
      (resolve-all sources joins finalize?))]
    [(ast:group-by columns)
     (ast:group-by (resolve-all sources columns finalize?))]
    [(ast:join type lateral? with constraint)
     (ast:join
      type lateral?
      (resolve sources with finalize?)
      (maybe-resolve sources constraint finalize?))]
    [(ast:order-by orderings)
     (ast:order-by (resolve-orderings sources orderings finalize?))]
    [(ast:returning es)
     (ast:returning (resolve-all sources es finalize?))]
    [(ast:where e)
     (ast:where (resolve sources e finalize?))]
    [(ast:union stmt)
     (ast:union (resolve sources stmt finalize?))]

    ;; expressions
    [(ast:app f args)
     (ast:app
      (resolve sources f finalize?)
      (resolve-all sources args finalize?))]
    [(ast:as e alias)
     (ast:as (resolve sources e finalize?) alias)]
    [(ast:case-e cases else-case)
     (ast:case-e
      (resolve-pairs sources cases finalize?)
      (maybe-resolve sources else-case finalize?))]
    [(ast:column (? symbol? id))
     (ast:column (get-column-name id finalize?))]
    [(ast:column e)
     (ast:column (resolve sources e finalize?))]
    [(ast:qualified parent (? symbol? id))
     (ast:qualified parent (resolve-qualified sources parent id finalize?))]
    [(ast:qualified parent name)
     (ast:qualified parent (resolve sources name finalize?))]
    [(ast:scalar (? pair? items))
     (ast:scalar (resolve-all sources items finalize?))]
    [(ast:scalar (? vector? items))
     (ast:scalar
      (for/vector #:length (vector-length items)
                  ([item (in-vector items)])
        (resolve sources item finalize?)))]
    [(ast:subquery stmt)
     (ast:subquery (resolve sources stmt finalize?))]

    [_ e]))

(define (resolve-all sources es [finalize? #f])
  (for/list ([e (in-list es)])
    (resolve sources e finalize?)))

(define (maybe-resolve sources e [finalize? #f])
  (and e (resolve sources e finalize?)))

(define (resolve-pairs sources pairs [finalize? #f])
  (for/list ([p (in-list pairs)])
    (match-define (cons a b) p)
    (cons
     (resolve sources a finalize?)
     (resolve sources b finalize?))))

(define (resolve-orderings sources orderings [finalize? #f])
  (for/list ([o (in-list orderings)])
    (match-define (cons column order) o)
    (cons (resolve sources column finalize?) order)))

(define (resolve-query q)
  (resolve
   (query-sources q)
   (query-stmt q)))

(define (resolve-assignment-column sources alias column)
  (match column
    [(ast:column (? symbol? id))
     (ast:column (resolve-qualified sources alias id #t))]
    [_ column]))

(define (resolve-qualified sources parent id finalize?)
  (define alias
    (if (symbol? parent)
        (symbol->string parent)
        parent))
  (cond
    ;; An alias may show up in sources as #f to signal that there is
    ;; nothing to punt on for that alias. For now, any field not found
    ;; on a schema is converted using id->column-name. In future, we
    ;; might decide to be stricter about this.
    [(hash-has-key? sources alias)
     (define s (hash-ref sources alias))
     (or (and s (resolve-field s id))
         (id->column-name id))]
    [else
     (get-column-name id finalize?)]))

(define (resolve-field s id)
  (for/first ([f (in-list (schema-fields s))]
              #:when (eq? id (field-id f)))
    (field-name f)))

(define (get-column-name id finalize?)
  (if finalize? (id->column-name id) id))
