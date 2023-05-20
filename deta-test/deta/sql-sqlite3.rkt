#lang racket/base

(require deta
         deta/private/dialect/dialect
         deta/private/dialect/sqlite3
         (only-in deta/private/query
                  query-stmt)
         rackunit
         threading)

(provide
 sql-tests)

(define-check (check-emitted q expected)
  (define-values (query _)
    (dialect-emit-query sqlite3-dialect (query-stmt q)))

  (with-check-info
    (['query query]
     ['expected expected])
    (unless (equal? query expected)
      (fail-check))))

(define sql-tests
  (test-suite
   "sqlite3-sql"

   (test-suite
    "functions"

    (check-emitted
     (~> (from "books" #:as b)
         (where (< b.published-at (date "now"))))
     "SELECT * FROM books AS b WHERE b.published_at < (DATE('now'))")

    (check-emitted
     (~> (from "books" #:as b)
         (where (< b.published-at (date "now" "start of month" "+1 month" "-1 day"))))
     "SELECT * FROM books AS b WHERE b.published_at < (DATE('now', 'start of month', '+1 month', '-1 day'))")

    (check-emitted
     (~> (from "books" #:as b)
         (where (< b.published-at (time "now"))))
     "SELECT * FROM books AS b WHERE b.published_at < (TIME('now'))")

    (check-emitted
     (~> (from "books" #:as b)
         (where (< b.published-at (datetime "now"))))
     "SELECT * FROM books AS b WHERE b.published_at < (DATETIME('now'))"))

   (test-suite
    "limit"

    (check-emitted
     (~> (from "books" #:as b)
         (limit 20))

     "SELECT * FROM books AS b LIMIT 20")

    (check-emitted
     (~> (from "books" #:as b)
         (offset 10)
         (limit 20))

     "SELECT * FROM books AS b LIMIT 20 OFFSET 10"))

   (test-suite
    "returning"

    (check-emitted
     (~> (from "books" #:as b)
         (update [published-at (date "now")])
         (returning title))

     "UPDATE books AS b SET published_at = DATE('now') RETURNING TITLE"))

   (test-suite
    "union"

    (check-emitted
     (~> (select _ 1)
         (union (select _ 2))
         (union (select _ 3)))
     "SELECT 1 UNION SELECT 2 UNION SELECT 3"))))

(module+ test
  (require rackunit/text-ui)
  (run-tests sql-tests))
