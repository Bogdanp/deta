#lang racket/base

(require racket/generic)

(provide
 gen:type
 type?
 type-contract
 type-declaration
 type-load
 type-dump)

(define-generics type
  (type-contract type)
  (type-declaration type dialect)
  (type-load type dialect v)
  (type-dump type dialect v))
