#lang racket/base

(require deta
         deta/adapter/adapter
         deta/adapter/sqlite3
         deta/query/struct
         (prefix-in ast: deta/private/ast)
         racket/format
         rackunit
         threading)

(provide
 sql-tests)

(define (query->stmt q)
  (cond
    [(query? q) (query-stmt q)]
    [else q]))

(define-check (check-emitted q expected)
  (define-values (query _)
    (adapter-emit-query sqlite3-adapter (query->stmt q)))

  (check-equal? query expected))

(define-check (check-emitted/placeholders q expected-query expected-placeholders)
  (define-values (query args)
    (adapter-emit-query sqlite3-adapter (query->stmt q)))

  (check-equal? query expected-query)
  (check-equal? args expected-placeholders))

(define sql-tests
  (test-suite
   "sqlite3-sql"

   (test-suite
    "fetch"

    (check-emitted (~> (from "books" #:as b)
                       (fetch 20))
                   "SELECT * FROM \"books\" AS \"b\" LIMIT 20")

    (check-emitted (~> (from "books" #:as b)
                       (offset 10)
                       (fetch 20))
                   "SELECT * FROM \"books\" AS \"b\" LIMIT 20 OFFSET 10"))))

(module+ test
  (require rackunit/text-ui)
  (run-tests sql-tests))
