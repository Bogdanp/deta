#lang racket/base

(require racket/contract
         (prefix-in ast: "../private/ast.rkt")
         "../schema.rkt")

(provide
 make-empty-query

 (contract-out
  [struct query ([schema (or/c false/c schema?)]
                 [stmt ast:select?])]))

(struct query (schema stmt)
  #:transparent)

(define (make-empty-query)
  (query #f (ast:select null #f #f)))
