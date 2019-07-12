#lang racket/base

(require racket/contract
         "../ast.rkt"
         "../private/field.rkt"
         "../schema.rkt")

(provide
 from)

(define/contract (from schema-or-name #:as alias)
  (-> (or/c schema? symbol?) #:as symbol? select-stmt?)

  (define schema (schema-registry-lookup schema-or-name))
  (define alias:str (symbol->string alias))

  (select-stmt
   (from-clause schema (alias-expr (table-expr (schema-table-name schema)) alias:str))
   (for/list ([f (in-list (schema-fields schema))])
     (column-expr (qualified-name alias:str (field-name f))))
   #f))
