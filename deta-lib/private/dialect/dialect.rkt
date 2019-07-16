#lang racket/base

(require racket/generic)

(provide
 gen:dialect
 dialect?
 dialect-name
 dialect-supports-returning?
 dialect-last-id-query
 dialect-emit-ddl
 dialect-emit-query)

(define-generics dialect
  (dialect-name dialect)
  (dialect-supports-returning? dialect)
  (dialect-last-id-query dialect)
  (dialect-emit-ddl dialect schema)
  (dialect-emit-query/impl dialect query))

(define (dialect-emit-query dialect stmt)
  (parameterize ([current-placeholders null])
    (values (dialect-emit-query/impl dialect stmt)
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
