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
 where
 and-where
 or-where)

(define/contract (as e name)
  (-> ast:expr? (or/c string? symbol?) ast:as?)
  (ast:as e (cond
              [(string? name) name]
              [(symbol? name) (symbol->string name)])))

(define/contract (from schema-or-name #:as alias)
  (-> (or/c schema? symbol?) #:as symbol? query?)

  (define schema (schema-registry-lookup schema-or-name))
  (define alias:str (symbol->string alias))

  (query schema
         (ast:select
          (ast:from (ast:as (ast:table (schema-table-name schema)) alias:str))
          (for/list ([f (in-list (schema-fields schema))])
            (ast:column (ast:qualified alias:str (field-name f))))
          #f)))

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
