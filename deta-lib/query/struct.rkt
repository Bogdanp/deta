#lang racket/base

(require db
         racket/class
         racket/contract
         racket/struct
         "../adapter/adapter.rkt"
         "../adapter/connection.rkt"
         "../adapter/postgresql.rkt"
         (prefix-in ast: "../private/ast.rkt")
         "../schema.rkt")

(provide
 make-empty-query

 (contract-out
  [struct query ([schema (or/c false/c schema?)]
                 [stmt (or/c ast:select?
                             ast:update?)])]))

(struct query (schema stmt)
  #:transparent
  #:property prop:statement
  (lambda (self c)
    (define adapter (connection-adapter c))
    (define-values (query args)
      (adapter-emit-query adapter (query-stmt self)))

    (define stmt
      (send c prepare 'query query #t))

    (cond
      [(null? args) stmt]
      [else (send stmt bind 'query args)]))

  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (self) 'query)
   (lambda (self)
     (define-values (query args)
       (adapter-emit-query postgresql-adapter (query-stmt self)))
     (cons query args))))

(define (make-empty-query)
  (query #f (ast:make-select)))
