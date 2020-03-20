#lang racket/base

(require db
         (only-in racket/class send)
         racket/contract
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
 (struct-out query))

(struct query (schema stmt)
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
   (lambda (self) 'query)
   (lambda (self)
     (define-values (query args)
       (dialect-emit-query postgresql-dialect (query-stmt self)))
     (cons query args))))

(define (make-empty-query)
  (query #f (ast:make-select)))

(define (make-query stmt #:schema [schema #f])
  (query schema stmt))


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
 delete
 from
 group-by
 join
 limit
 offset
 or-where
 union
 order-by
 project-onto
 returning
 select
 subquery
 update
 where)

(define/contract (delete q)
  (-> select-query? query?)
  (match q
    [(query schema (and (struct* ast:select ([from (ast:from (list table) _)]
                                             [where where]))
                        stmt))
     (when (contains-subquery? table)
       (raise-argument-error 'delete "a real table to delete from" table))

     (query schema (ast:make-delete
                    #:from (ast:make-from #:tables (list table))
                    #:where where))]))

(define/contract (from source #:as alias)
  (-> any/c #:as symbol? query?)

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
       #:columns (for/list ([f (in-list (schema-fields schema))])
                   (ast:column (ast:qualified alias:str (field-name f))))))]

    [(ast:subquery? source)
     (make-query
      (ast:make-select
       #:from (ast:make-from #:tables (list (ast:as source alias:str)))))]

    [else
     (raise-argument-error 'form "a table name, a schema name or a subquery")]))

(define/contract (join q
                       #:type type
                       #:with schema-or-name
                       #:as alias
                       #:on constraint)
  (-> select-query?
      #:type (or/c 'inner 'left 'right 'full 'cross)
      #:with (or/c schema? string? symbol?)
      #:as symbol?
      #:on ast:expr?
      query?)

  (define alias:str (symbol->string alias))
  (define table-name
    (cond
      [(string? schema-or-name) schema-or-name]
      [else
       (define schema (schema-registry-lookup schema-or-name))
       (schema-table schema)]))

  (match q
    [(query schema stmt)
     (query schema (struct-copy ast:select stmt
                                [from (add-join
                                       (ast:select-from stmt)
                                       (ast:join type (ast:as (ast:table table-name) alias) constraint))]))]))

(define/contract (select q column0 . columns)
  (-> query? ast:expr? ast:expr? ... query?)
  (match q
    [(query _ stmt)
     (query #f (struct-copy ast:select stmt [columns (cons column0 columns)]))]))

(define/contract (limit q n)
  (-> query? (or/c ast:scalar? ast:placeholder?) query?)
  (match q
    [(query schema stmt)
     (query schema (struct-copy ast:select stmt [limit (ast:limit n)]))]))

(define/contract (group-by q column0 . columns)
  (-> select-query? ast:expr? ast:expr? ... query?)

  (define all-columns
    (cons column0 columns))

  (match q
    [(query schema (and (struct* ast:select ([group-by #f])) stmt))
     (query schema (struct-copy ast:select stmt [group-by (ast:group-by all-columns)]))]

    [(query schema (and (struct* ast:select ([group-by (ast:group-by existing-columns)])) stmt))
     (query schema (struct-copy ast:select stmt [group-by (ast:group-by (append existing-columns all-columns))]))]))

(define/contract (offset q n)
  (-> query? (or/c ast:scalar? ast:placeholder?) query?)
  (match q
    [(query schema stmt)
     (query schema (struct-copy ast:select stmt [offset (ast:offset n)]))]))

(define order-by-pair/c
  (cons/c ast:expr? (or/c 'asc 'desc)))

(define/contract (union q1 q2)
  (-> select-query? select-query? query?)

  (define (union* s1 s2)
    (match s1
      [(struct* ast:select ([union #f]))
       (struct-copy ast:select s1 [union (ast:union s2)])]

      [(struct* ast:select ([union u]))
       (struct-copy ast:select s1 [union (ast:union (union* (ast:union-stmt u) s2))])]))

  (match q1
    [(query schema (and (struct* ast:select ([union #f])) stmt))
     (query schema (struct-copy ast:select stmt [union (ast:union (query-stmt q2))]))]

    [(query schema (and (struct* ast:select ([union u])) stmt))
     (query schema (struct-copy ast:select stmt [union (ast:union (union* (ast:union-stmt u) (query-stmt q2)))]))]))

(define/contract (order-by q pair0 . pairs)
  (-> select-query? order-by-pair/c order-by-pair/c ... query?)

  (define all-pairs
    (cons pair0 pairs))

  (match q
    [(query schema (and (struct* ast:select ([order-by #f])) stmt))
     (query schema (struct-copy ast:select stmt [order-by (ast:order-by all-pairs)]))]

    [(query schema (and (struct* ast:select ([order-by (ast:order-by existing-pairs)])) stmt))
     (query schema (struct-copy ast:select stmt [order-by (ast:order-by (append existing-pairs all-pairs))]))]))

(define/contract (project-onto q s)
  (-> query? schema? query?)
  (struct-copy query q [schema s]))

(define/contract (returning q e0 . es)
  (-> query? ast:expr? ast:expr? ... query?)

  (define all-exprs
    (cons e0 es))

  (define (append-exprs e)
    (match e
      [#f
       (ast:returning all-exprs)]

      [(ast:returning existing-exprs)
       (ast:returning (append existing-exprs all-exprs))]))

  (match q
    [(query schema (and (? ast:insert?) stmt))
     (query schema (struct-copy ast:insert stmt [returning (append-exprs (ast:insert-returning stmt))]))]

    [(query schema (and (? ast:update?) stmt))
     (query schema (struct-copy ast:update stmt [returning (append-exprs (ast:update-returning stmt))]))]

    [(query schema (and (? ast:delete?) stmt))
     (query schema (struct-copy ast:delete stmt [returning (append-exprs (ast:delete-returning stmt))]))]))

(define/contract (subquery q)
  (-> select-query? ast:subquery?)
  (ast:subquery (query-stmt q)))

(define/contract (update q assignment0 . assignments)
  (-> select-query? assignment/c assignment/c ... query?)

  (define all-assignments
    (cons assignment0 assignments))

  (match q
    [(query schema (struct* ast:select
                            ([from (ast:from (list table) _)]
                             [where where])))
     (when (contains-subquery? table)
       (raise-argument-error 'update "a table to update" table))

     (query schema
            (ast:make-update
             #:table table
             #:assignments (ast:assignments all-assignments)
             #:where where))]))

(define/contract (where q e)
  (-> query? ast:expr? query?)
  (match q
    [(query schema (and (struct* ast:select ([where #f])) stmt))
     (query schema (struct-copy ast:select stmt [where (ast:where e)]))]

    [(query schema (and (struct* ast:update ([where #f])) stmt))
     (query schema (struct-copy ast:update stmt [where (ast:where e)]))]

    [(query schema (and (struct* ast:delete ([where #f])) stmt))
     (query schema (struct-copy ast:delete stmt [where (ast:where e)]))]

    [(query schema (and (struct* ast:select ([where (ast:where e0)])) stmt))
     (query schema (struct-copy ast:select stmt [where (ast:where (ast:app (ast:ident 'and) (list e0 e)))]))]

    [(query schema (and (struct* ast:update ([where (ast:where e0)])) stmt))
     (query schema (struct-copy ast:update stmt [where (ast:where (ast:app (ast:ident 'and) (list e0 e)))]))]

    [(query schema (and (struct* ast:delete ([where (ast:where e0)])) stmt))
     (query schema (struct-copy ast:delete stmt [where (ast:where (ast:app (ast:ident 'and) (list e0 e)))]))]))

(define/contract (or-where q e)
  (-> query? ast:expr? query?)
  (match q
    [(query schema (and (struct* ast:select ([where #f])) stmt))
     (query schema (struct-copy ast:select stmt [where (ast:where e)]))]

    [(query schema (and (struct* ast:update ([where #f])) stmt))
     (query schema (struct-copy ast:update stmt [where (ast:where e)]))]

    [(query schema (and (struct* ast:delete ([where #f])) stmt))
     (query schema (struct-copy ast:delete stmt [where (ast:where e)]))]

    [(query schema (and (struct* ast:select ([where (ast:where e0)])) stmt))
     (query schema (struct-copy ast:select stmt [where (ast:where (ast:app (ast:ident 'or) (list e0 e)))]))]

    [(query schema (and (struct* ast:update ([where (ast:where e0)])) stmt))
     (query schema (struct-copy ast:update stmt [where (ast:where (ast:app (ast:ident 'or) (list e0 e)))]))]

    [(query schema (and (struct* ast:delete ([where (ast:where e0)])) stmt))
     (query schema (struct-copy ast:delete stmt [where (ast:where (ast:app (ast:ident 'or) (list e0 e)))]))]))
