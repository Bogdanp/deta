#lang racket/base

(require racket/generic)

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
  (adapter-emit-query/impl adapter query))

(define (adapter-emit-query adapter stmt)
  (parameterize ([current-placeholders null])
    (values (adapter-emit-query/impl adapter stmt)
            (reverse (current-placeholders)))))


;; placeholders ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 track-placeholder!)

(define current-placeholders
  (make-parameter null))

(define (track-placeholder! v)
  (define placeholders (cons v (current-placeholders)))
  (current-placeholders placeholders)
  (length placeholders))
