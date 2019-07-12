#lang racket/base

(require racket/contract
         (prefix-in ast: "../ast.rkt")
         "../private/field.rkt"
         "../schema.rkt"
         "struct.rkt")

(provide
 from)

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
