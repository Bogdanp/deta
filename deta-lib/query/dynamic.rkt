#lang racket/base

(require racket/contract
         racket/match
         (prefix-in ast: "../private/ast.rkt")
         "../private/field.rkt"
         "../schema.rkt"
         "struct.rkt")

(provide
 as
 from
 group-by
 project-onto
 select
 where
 and-where
 or-where)

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

(define/contract (select q . columns)
  (-> query? ast:expr? ... query?)
  (match q
    [(query _ stmt)
     (query #f (struct-copy ast:select stmt [columns columns]))]))

(define/contract (group-by q . columns)
  (-> query? ast:expr? ... query?)
  (match q
    [(query _ stmt)
     (query #f (struct-copy ast:select stmt [group-by (ast:group-by columns)]))]))

(define/contract (project-onto q s)
  (-> query? schema? query?)
  (struct-copy query q [projection s]))

(define/contract (where q e)
  (-> query? ast:expr? query?)
  (match q
    [(query schema stmt)
     (query schema (struct-copy ast:select stmt [where (ast:where e)]))]))

(define/contract (and-where q e)
  (-> query? ast:expr? query?)
  (match q
    [(query schema (and (struct* ast:select ([where #f])) stmt))
     (query schema (struct-copy ast:select stmt [where e]))]

    [(query schema (and (struct* ast:select ([where (ast:where e0)])) stmt))
     (query schema (struct-copy ast:select stmt [where (ast:where (ast:app (ast:name 'and) (list e0 e)))]))]))

(define/contract (or-where q e)
  (-> query? ast:expr? query?)
  (match q
    [(query schema (and (struct* ast:select ([where #f])) stmt))
     (query schema (struct-copy ast:select stmt [where e]))]

    [(query schema (and (struct* ast:select ([where (ast:where e0)])) stmt))
     (query schema (struct-copy ast:select stmt [where (ast:where (ast:app (ast:name 'or) (list e0 e)))]))]))
