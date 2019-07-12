#lang racket/base

(require racket/contract
         racket/generic)

(provide
 gen:adapter
 adapter?
 adapter-supports-returning?
 adapter-last-id-query
 adapter-emit-ddl
 adapter-emit-query)

(define-generics adapter
  (adapter-supports-returning? adapter)
  (adapter-last-id-query adapter)
  (adapter-emit-ddl adapter schema)
  (adapter-emit-query adapter query))
