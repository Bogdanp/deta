#lang at-exp racket/base

(require deta
         deta/private/dialect/dialect
         deta/private/dialect/sqlite3
         (only-in deta/private/query
                  query-stmt)
         racket/format
         rackunit
         threading)

(provide
 sql-tests)

(define-check (check-emitted q expected)
  (define-values (query _)
    (dialect-emit-query sqlite3-dialect (query-stmt q)))

  (check-equal? query expected))

(define sql-tests
  (test-suite
   "sqlite3-sql"

   (test-suite
    "limit"

    (check-emitted
     (~> (from "books" #:as b)
         (limit 20))

     @~a{SELECT * FROM "books" AS "b" LIMIT 20})

    (check-emitted
     (~> (from "books" #:as b)
         (offset 10)
         (limit 20))

     @~a{SELECT * FROM "books" AS "b" LIMIT 20 OFFSET 10}))))

(module+ test
  (require rackunit/text-ui)
  (run-tests sql-tests))
