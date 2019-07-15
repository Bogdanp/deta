#lang racket/base

(require db
         deta
         deta/private/field
         deta/private/schema
         deta/private/type
         gregor
         rackunit)

(define type-tests
  (test-suite
   "type"

   (test-suite
    "array/f"

    (for ([dialect '(postgresql sqlite3)]
          [expected (list (vector (sql-date 1996 5 29)
                                  (sql-date 2019 5 29))
                          (vector "1996-05-29"
                                  "2019-05-29"))])
      (test-case (format "can dump and load dumped values for ~a" dialect)
        (define initial
          (vector (date 1996 5 29)
                  (date 2019 5 29)))

        (define type (array/f date/f))
        (define dumped
          (type-dump type dialect initial))

        (check-equal? dumped expected)

        (define loaded
          (type-load type dialect dumped))

        (check-equal? loaded initial))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests type-tests))
