#lang racket/base

(require db
         racket/contract
         "dialect/dialect.rkt"
         "dialect/postgresql.rkt"
         "dialect/sqlite3.rkt")

(provide
 connection-dialect)

(define/contract (connection-dialect conn)
  (-> connection? dialect?)
  (case (dbsystem-name (connection-dbsystem conn))
    [(postgresql) postgresql-dialect]
    [(sqlite3)    sqlite3-dialect]
    [else         (error 'connection-dialect "dialect not supported")]))
