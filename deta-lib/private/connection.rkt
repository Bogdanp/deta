#lang racket/base

(require db
         racket/contract/base
         "dialect/dialect.rkt"
         "dialect/postgresql.rkt"
         "dialect/sqlite3.rkt")

(provide
 (contract-out
  [connection-dialect (-> connection? dialect?)]))

(define (connection-dialect conn)
  (case (dbsystem-name (connection-dbsystem conn))
    [(postgresql) postgresql-dialect]
    [(sqlite3)    sqlite3-dialect]
    [else         (error 'connection-dialect "dialect not supported")]))
