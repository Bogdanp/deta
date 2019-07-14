#lang racket/base

(require db
         deta
         racket/port
         rackunit)

(provide
 query-tests)

(define conn
  (sqlite3-connect #:database 'memory))

(define query-tests
  (test-suite
   "query"

   (test-suite
    "prop:statement"

    (test-case "queries can be executed by the standard query functions"
      (check-equal? (query-value conn (select 1)) 1))

    (test-case "queries with placeholders can be executed by the standard query functions"
      (check-equal? (query-value conn (select ,1)) 1)))

   (test-suite
    "prop:custom-write"

    (test-case "prints queries"
      (check-equal?
       (with-output-to-string
        (lambda _
          (display (select 1))))
       "#<query: SELECT 1>")

      (check-equal?
       (with-output-to-string
        (lambda _
          (display (select ,1))))
       "#<query: SELECT $1 1>")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests query-tests))
