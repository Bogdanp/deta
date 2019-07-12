#lang racket/base

(require racket/generic)

(provide
 gen:type
 type?
 type-contract
 type-load
 type-dump)

(define-generics type
  (type-contract type)
  (type-load type f v)
  (type-dump type f))
