#lang racket/base

(require racket/contract
         "../private/ast.rkt"
         "../schema.rkt")

(provide
 (contract-out
  [struct query ([schema schema?]
                 [stmt select?])]))

(struct query (schema stmt)
  #:transparent)
