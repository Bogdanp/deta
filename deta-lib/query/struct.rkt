#lang racket/base

(require racket/contract
         "../ast.rkt"
         "../schema.rkt")

(provide
 (contract-out
  [struct query ([schema schema?]
                 [stmt stmt?])]))

(struct query (schema stmt)
  #:transparent)
