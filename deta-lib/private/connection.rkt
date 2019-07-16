#lang racket/base

(require db
         racket/contract
         racket/match
         "dialect/dialect.rkt"
         "dialect/postgresql.rkt"
         "dialect/sqlite3.rkt")

(provide
 connection-dialect)

(define/contract (connection-dialect conn)
  (-> connection? dialect?)
  (match (dbsystem-name (connection-dbsystem conn))
    ['postgresql postgresql-dialect]
    ['sqlite3    sqlite3-dialect]
    [_           (error 'connection-dialect "dialect not supported")]))
