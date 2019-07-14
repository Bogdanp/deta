#lang racket/base

(require db
         racket/contract
         racket/match
         "adapter.rkt"
         "postgresql.rkt"
         "sqlite3.rkt")

(provide
 connection-adapter)

(define/contract (connection-adapter conn)
  (-> connection? adapter?)
  (match (dbsystem-name (connection-dbsystem conn))
    ['postgresql postgresql-adapter]
    ['sqlite3    sqlite3-adapter]
    [_           (error 'connection-adapter "this connection type is not supported")]))
