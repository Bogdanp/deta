#lang at-exp racket/base

(require racket/contract
         racket/format
         racket/match
         racket/port
         racket/string
         "../private/field.rkt"
         "../schema.rkt"
         "adapter.rkt"
         "ast.rkt"
         "standard.rkt")

(provide
 postgresql-adapter?
 postgresql-adapter)

(define-values (postgresql-adapter? postgresql-adapter)
  (let ()
    (struct postgresql-adapter ()
      #:methods gen:adapter
      [(define (adapter-supports-returning? _) #t)

       (define/contract (adapter-emit-query _ stmt)
         (-> adapter? stmt? string?)
         (emit/standard #:supports-returning? #t stmt))])

    (values postgresql-adapter?
            (postgresql-adapter))))
