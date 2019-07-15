#lang racket/base

(require deta
         deta/private/adapter/adapter
         deta/private/adapter/sqlite3
         (only-in deta/private/query query-stmt)
         (prefix-in ast: deta/private/ast)
         racket/format
         rackunit
         threading)

(provide
 sql-tests)

(define-check (check-emitted q expected)
  (define-values (query _)
    (adapter-emit-query sqlite3-adapter (query-stmt q)))

  (check-equal? query expected))

(define sql-tests
  (test-suite
   "sqlite3-sql"

   (test-suite
    "limit"

    (check-emitted (~> (from "books" #:as b)
                       (limit 20))
                   "SELECT * FROM \"books\" AS \"b\" LIMIT 20")

    (check-emitted (~> (from "books" #:as b)
                       (offset 10)
                       (limit 20))
                   "SELECT * FROM \"books\" AS \"b\" LIMIT 20 OFFSET 10"))))

(module+ test
  (require rackunit/text-ui)
  (run-tests sql-tests))
