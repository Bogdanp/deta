#lang racket/base

(require racket/contract
         (prefix-in ast: "../private/ast.rkt")
         "../schema.rkt")

(provide
 make-empty-query

 (contract-out
  [struct query ([projection (or/c false/c schema?)]
                 [stmt ast:select?])]))

(struct query (projection stmt)
  #:transparent)

(define (make-empty-query)
  (query #f (ast:make-select)))
