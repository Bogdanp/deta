#lang racket/base

(define-syntax-rule (reprovide e ...)
  (begin
    (require e ...)
    (provide (all-from-out e ...))))

(reprovide
 "schema.rkt"
 "query.rkt"
 "type.rkt")
