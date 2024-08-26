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
 (struct-out query))

(struct opts (project-virtual-fields?)
  #:transparent)

(struct query (schema opts stmt)
  #:transparent
  #:property prop:statement
  (lambda (self c)
    (define dialect (connection-dialect c))
    (define-values (query args)
      (dialect-emit-query dialect (query-stmt self)))

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
       (dialect-emit-query postgresql-dialect (query-stmt self)))
     (cons query args))))

(define (make-empty-query)
  (query #f (opts #f) (ast:make-select)))

(define (make-query stmt
                    #:schema [schema #f]
                    #:project-virtual-fields? [project-virtual? #f])
  (query schema (opts project-virtual?) stmt))


;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (select-query? q)
  (and/c query? (ast:select? (query-stmt q))))

(define (contains-subquery? f)
  (match f
    [(ast:subquery _)             #t]
    [(ast:as (? ast:subquery?) _) #t]
    [_                            #f]))

(define (add-join f j)
  (struct-copy ast:from f [joins (append (ast:from-joins f) (list j))]))

(define assignment/c
  (cons/c ast:column? ast:expr?))


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
        [#:distinct? boolean?]
        #:rest (listof ast:expr?)
        query?)]
  [select-for-schema
   (-> query?
       (or/c schema? symbol?)
       string?
       (hash/c symbol? ast:expr?)
       query?)]
  [subquery (-> select-query? ast:subquery?)]
  [union (-> select-query? select-query? query?)]
  [update (-> select-query? assignment/c assignment/c ... query?)]
  [where (-> query? ast:expr? query?)]))

(define (delete q)
  (match q
    [(query schema opts (struct* ast:select ([from (ast:from (list table) _)]
                                             [where where])))
     (when (contains-subquery? table)
       (raise-argument-error 'delete "a real table to delete from" table))

     (query schema opts (ast:make-delete
                         #:from (ast:make-from #:tables (list table))
                         #:where where))]))

(define (from source #:as alias)
  (define alias:str (symbol->string alias))
  (cond
    [(string? source)
     (make-query
      (ast:make-select
       #:from (ast:make-from
               #:tables (list (ast:as (ast:table source) alias:str)))))]

    [(symbol? source)
     (define schema (schema-registry-lookup source))
     (make-query
      #:schema schema
      (ast:make-select
       #:from (ast:make-from
               #:tables (list (ast:as (ast:table (schema-table schema)) alias:str)))
       #:columns (for/list ([f (in-list (schema-fields/nonvirtual schema))])
                   (ast:column (ast:qualified alias:str (field-name f))))))]

    [(ast:subquery? source)
     (make-query
      (ast:make-select
       #:from (ast:make-from #:tables (list (ast:as source alias:str)))))]

    [else
     (raise-argument-error 'form "a table name, a schema name or a subquery" source)]))

(define (join q
              #:type type
              #:lateral? lateral?
              #:with tbl-e
              #:as alias
              #:on constraint)
  (define tbl-clause
    (match tbl-e
      [(? ast:subquery?) tbl-e]
      [_ (ast:table
          (cond
            [(string? tbl-e) tbl-e]
            [else (schema-table (schema-registry-lookup tbl-e))]))]))

  (match q
    [(query schema opts stmt)
     (query schema opts (struct-copy ast:select stmt
                                     [from (add-join
                                            (ast:select-from stmt)
                                            (ast:join type lateral? (ast:as tbl-clause alias) constraint))]))]))

(define (select q column0
                #:distinct? [distinct? #f]
                . columns)
  (match q
    [(query _  opts stmt)
     (query #f opts (struct-copy ast:select stmt
                                 [distinct? distinct?]
                                 [columns (cons column0 columns)]))]))

(define (select-for-schema q schema-or-id tbl-alias customizations)
  (define s (schema-registry-lookup schema-or-id))
  (define q* (apply select q (for/list ([fld (schema-fields s)])
                               (hash-ref customizations
                                         (field-id fld)
                                         (lambda ()
                                           (ast:qualified tbl-alias (field-name fld)))))))
  (project-onto q* s))

(define (limit q n)
  (match q
    [(query schema opts stmt)
     (query schema opts (struct-copy ast:select stmt [limit (ast:limit n)]))]))

(define (group-by q column0 . columns)
  (define all-columns
    (cons column0 columns))

  (match q
    [(query schema opts (and (struct* ast:select ([group-by #f])) stmt))
     (query schema opts (struct-copy ast:select stmt [group-by (ast:group-by all-columns)]))]

    [(query schema opts (and (struct* ast:select ([group-by (ast:group-by existing-columns)])) stmt))
     (query schema opts (struct-copy ast:select stmt [group-by (ast:group-by (append existing-columns all-columns))]))]))

(define (offset q n)
  (match q
    [(query schema opts stmt)
     (query schema opts (struct-copy ast:select stmt [offset (ast:offset n)]))]))

(define ordering/c
  (list/c ast:expr? (or/c 'asc 'desc) (or/c #f 'nulls-first 'nulls-last)))

(define (union q1 q2)
  (define (union* s1 s2)
    (match s1
      [(struct* ast:select ([union #f]))
       (struct-copy ast:select s1 [union (ast:union s2)])]

      [(struct* ast:select ([union u]))
       (struct-copy ast:select s1 [union (ast:union (union* (ast:union-stmt u) s2))])]))

  (match q1
    [(query schema opts (and (struct* ast:select ([union #f])) stmt))
     (query schema opts (struct-copy ast:select stmt [union (ast:union (query-stmt q2))]))]

    [(query schema opts (and (struct* ast:select ([union u])) stmt))
     (query schema opts (struct-copy ast:select stmt [union (ast:union (union* (ast:union-stmt u) (query-stmt q2)))]))]))

(define (order-by q ordering0 . orderings)
  (define all-orderings
    (cons ordering0 orderings))

  (match q
    [(query schema opts (and (struct* ast:select ([order-by #f])) stmt))
     (query schema opts (struct-copy ast:select stmt [order-by (ast:order-by all-orderings)]))]

    [(query schema opts (and (struct* ast:select ([order-by (ast:order-by existing-orderings)])) stmt))
     (query schema opts (struct-copy ast:select stmt [order-by (ast:order-by (append existing-orderings all-orderings))]))]))

(define (project-onto q s)
  (struct-copy query q [schema s]))

(define (project-virtual-fields q)
  (struct-copy query q [opts (opts #t)]))

(define (returning q e0 . es)
  (define all-exprs
    (cons e0 es))

  (define (append-exprs e)
    (match e
      [#f
       (ast:returning all-exprs)]

      [(ast:returning existing-exprs)
       (ast:returning (append existing-exprs all-exprs))]))

  (match q
    [(query schema opts (and (? ast:insert?) stmt))
     (query schema opts (struct-copy ast:insert stmt [returning (append-exprs (ast:insert-returning stmt))]))]

    [(query schema opts (and (? ast:update?) stmt))
     (query schema opts (struct-copy ast:update stmt [returning (append-exprs (ast:update-returning stmt))]))]

    [(query schema opts (and (? ast:delete?) stmt))
     (query schema opts (struct-copy ast:delete stmt [returning (append-exprs (ast:delete-returning stmt))]))]))

(define (subquery q)
  (ast:subquery (query-stmt q)))

(define (update q assignment0 . assignments)
  (define all-assignments
    (cons assignment0 assignments))

  (match q
    [(query schema opts (struct* ast:select
                                 ([from (ast:from (list table) _)]
                                  [where where])))
     (when (contains-subquery? table)
       (raise-argument-error 'update "a table to update" table))

     (query schema opts (ast:make-update
                         #:table table
                         #:assignments (ast:assignments all-assignments)
                         #:where where))]))

(define (where q e)
  (match q
    [(query schema opts (and (struct* ast:select ([where #f])) stmt))
     (query schema opts (struct-copy ast:select stmt [where (ast:where e)]))]

    [(query schema opts (and (struct* ast:update ([where #f])) stmt))
     (query schema opts (struct-copy ast:update stmt [where (ast:where e)]))]

    [(query schema opts (and (struct* ast:delete ([where #f])) stmt))
     (query schema opts (struct-copy ast:delete stmt [where (ast:where e)]))]

    [(query schema opts (and (struct* ast:select ([where (ast:where e0)])) stmt))
     (query schema opts (struct-copy ast:select stmt [where (ast:where (ast:app (ast:ident 'and) (list e0 e)))]))]

    [(query schema opts (and (struct* ast:update ([where (ast:where e0)])) stmt))
     (query schema opts (struct-copy ast:update stmt [where (ast:where (ast:app (ast:ident 'and) (list e0 e)))]))]

    [(query schema opts (and (struct* ast:delete ([where (ast:where e0)])) stmt))
     (query schema opts (struct-copy ast:delete stmt [where (ast:where (ast:app (ast:ident 'and) (list e0 e)))]))]))

(define (or-where q e)
  (match q
    [(query schema opts (and (struct* ast:select ([where #f])) stmt))
     (query schema opts (struct-copy ast:select stmt [where (ast:where e)]))]

    [(query schema opts (and (struct* ast:update ([where #f])) stmt))
     (query schema opts (struct-copy ast:update stmt [where (ast:where e)]))]

    [(query schema opts (and (struct* ast:delete ([where #f])) stmt))
     (query schema opts (struct-copy ast:delete stmt [where (ast:where e)]))]

    [(query schema opts (and (struct* ast:select ([where (ast:where e0)])) stmt))
     (query schema opts (struct-copy ast:select stmt [where (ast:where (ast:app (ast:ident 'or) (list e0 e)))]))]

    [(query schema opts (and (struct* ast:update ([where (ast:where e0)])) stmt))
     (query schema opts (struct-copy ast:update stmt [where (ast:where (ast:app (ast:ident 'or) (list e0 e)))]))]

    [(query schema opts (and (struct* ast:delete ([where (ast:where e0)])) stmt))
     (query schema opts (struct-copy ast:delete stmt [where (ast:where (ast:app (ast:ident 'or) (list e0 e)))]))]))
