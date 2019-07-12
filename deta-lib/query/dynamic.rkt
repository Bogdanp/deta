#lang racket/base

(require racket/contract
         (prefix-in ast: "../ast.rkt")
         "../private/field.rkt"
         "../schema.rkt")

(provide
 (contract-out
  [from (-> (or/c schema? symbol?) #:as symbol? ast:select?)]))

(define (from schema-or-name #:as alias)
  (define schema (schema-registry-lookup schema-or-name))
  (define alias:str (symbol->string alias))

  (ast:select
   (ast:from schema (ast:as (ast:table (schema-table-name schema)) alias:str))
   (for/list ([f (in-list (schema-fields schema))])
     (ast:column (ast:qualified alias:str (field-name f))))
   #f))
