#lang racket/base

(require racket/contract
         (prefix-in ast: "../private/ast.rkt")
         "../private/field.rkt"
         "../schema.rkt"
         "struct.rkt")

(provide
 (contract-out
  [as (-> ast:expr? (or/c string? symbol?) ast:as?)]
  [from (-> (or/c schema? symbol?) #:as symbol? query?)]))

(define (as e name)
  (ast:as e (cond
              [(string? name) name]
              [(symbol? name) (symbol->string name)])))

(define (from schema-or-name #:as alias)
  (define schema (schema-registry-lookup schema-or-name))
  (define alias:str (symbol->string alias))

  (query schema
         (ast:select
          (ast:from (ast:as (ast:table (schema-table-name schema)) alias:str))
          (for/list ([f (in-list (schema-fields schema))])
            (ast:column (ast:qualified alias:str (field-name f))))
          #f)))
