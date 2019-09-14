#lang racket/base

(require db
         racket/generic)

(provide
 gen:type
 type?
 type-contract
 type-declaration
 type-load
 type-load/null
 type-dump
 type-dump/null)

(define-generics type
  (type-contract type)
  (type-declaration type dialect)
  (type-load type dialect v)
  (type-dump type dialect v))

(define (type-load/null type dialect v)
  (if (sql-null? v)
      sql-null
      (type-load type dialect v)))

(define (type-dump/null type dialect v)
  (if (sql-null? v)
      sql-null
      (type-dump type dialect v)))
