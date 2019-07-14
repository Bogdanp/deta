#lang racket/base

(require racket/contract
         racket/match
         (prefix-in ast: "../private/ast.rkt")
         "../private/field.rkt"
         "../schema.rkt"
         "struct.rkt")

(provide
 and-where
 as
 from
 group-by
 limit
 offset
 or-where
 order-by
 project-onto
 select
 update
 where)

(define/contract (as e name)
  (-> ast:expr? (or/c string? symbol?) ast:as?)
  (ast:as e (cond
              [(string? name) name]
              [(symbol? name) (symbol->string name)])))

(define/contract (from schema-or-name #:as alias)
  (-> (or/c schema? string? symbol?) #:as symbol? query?)

  (define alias:str (symbol->string alias))
  (define-values (schema table-name columns)
    (cond
      [(string? schema-or-name)
       (values #f schema-or-name null)]

      [else
       (define schema (schema-registry-lookup schema-or-name))
       (values schema
               (schema-table-name schema)
               (for/list ([f (in-list (schema-fields schema))])
                 (ast:column (ast:qualified alias:str (field-name f)))))]))

  (query schema
         (ast:make-select
          #:columns columns
          #:from (ast:from (ast:as (ast:table table-name) alias:str)))))

(define/contract (select q column0 . columns)
  (-> query? ast:expr? ast:expr? ... query?)
  (match q
    [(query _ stmt)
     (query #f (struct-copy ast:select stmt [columns (cons column0 columns)]))]))

(define/contract (limit q n)
  (-> query? exact-integer? query?)
  (match q
    [(query schema stmt)
     (query schema (struct-copy ast:select stmt [limit (ast:limit n)]))]))

(define/contract (group-by q column0 . columns)
  (-> query? ast:expr? ast:expr? ... query?)
  (match q
    [(query schema stmt)
     (query schema (struct-copy ast:select stmt [group-by (ast:group-by (cons column0 columns))]))]))

(define/contract (offset q n)
  (-> query? exact-integer? query?)
  (match q
    [(query schema stmt)
     (query schema (struct-copy ast:select stmt [offset (ast:offset n)]))]))

(define order-by-pair/c
  (cons/c ast:expr? (or/c 'asc 'desc)))

(define/contract (order-by q pair0 . pairs)
  (-> query? order-by-pair/c order-by-pair/c ... query?)
  (match q
    [(query schema stmt)
     (query schema (struct-copy ast:select stmt [order-by (ast:order-by (cons pair0 pairs))]))]))

(define/contract (project-onto q s)
  (-> query? schema? query?)
  (struct-copy query q [schema s]))

(define/contract (update schema-or-name
                         #:as alias
                         #:set assignments
                         #:where [where #f])
  (->* ((or/c schema? string? symbol?)
        #:as symbol?
        #:set (listof (cons/c ast:expr? ast:expr?)))
       (#:where (or/c ast:expr?))
       query?)

  (define alias:str (symbol->string alias))
  (define-values (schema table-name)
    (cond
      [(string? schema-or-name)
       (values #f schema-or-name)]

      [else
       (define schema (schema-registry-lookup schema-or-name))
       (values schema (schema-table-name schema))]))

  (query schema
         (ast:make-update
          #:table (ast:as (ast:table table-name) alias:str)
          #:assignments (ast:assignments assignments)
          #:where where)))

(define/contract (where q e)
  (-> query? ast:expr? query?)
  (match q
    [(query schema (and (? ast:select?) stmt))
     (query schema (struct-copy ast:select stmt [where (ast:where e)]))]

    [(query schema (and (? ast:update?) stmt))
     (query schema (struct-copy ast:update stmt [where (ast:where e)]))]))

(define/contract (and-where q e)
  (-> query? ast:expr? query?)
  (match q
    [(query schema (and (struct* ast:select ([where #f])) stmt))
     (query schema (struct-copy ast:select stmt [where e]))]

    [(query schema (and (struct* ast:update ([where #f])) stmt))
     (query schema (struct-copy ast:update stmt [where e]))]

    [(query schema (and (struct* ast:select ([where (ast:where e0)])) stmt))
     (query schema (struct-copy ast:select stmt [where (ast:where (ast:app (ast:name 'and) (list e0 e)))]))]

    [(query schema (and (struct* ast:update ([where (ast:where e0)])) stmt))
     (query schema (struct-copy ast:update stmt [where (ast:where (ast:app (ast:name 'and) (list e0 e)))]))]))

(define/contract (or-where q e)
  (-> query? ast:expr? query?)
  (match q
    [(query schema (and (struct* ast:select ([where #f])) stmt))
     (query schema (struct-copy ast:select stmt [where e]))]

    [(query schema (and (struct* ast:update ([where #f])) stmt))
     (query schema (struct-copy ast:update stmt [where e]))]

    [(query schema (and (struct* ast:select ([where (ast:where e0)])) stmt))
     (query schema (struct-copy ast:select stmt [where (ast:where (ast:app (ast:name 'or) (list e0 e)))]))]

    [(query schema (and (struct* ast:update ([where (ast:where e0)])) stmt))
     (query schema (struct-copy ast:update stmt [where (ast:where (ast:app (ast:name 'or) (list e0 e)))]))]))
