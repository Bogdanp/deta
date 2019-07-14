#lang racket/base

(require db
         deta
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
      (check-equal? (query-value conn (select 1)) 1)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests query-tests))
