#lang racket/base

(require db
         db/util/postgresql
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
          [expected (list (list (sql-date 1996 5 29)
                                (sql-date 2019 5 29))
                          (list "1996-05-29"
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
          (type-load type dialect (list->pg-array dumped)))

        (check-equal? loaded initial))))

   (test-suite
    "uuid/f"

    (test-case "raises when invalid uuid suppiled"
      (check-exn
       exn:fail:contract?
       (lambda ()
         (define-schema with-uuid
           ([uuid uuid/f]))

         (make-with-uuid
          ("invalid-uuid")))))

    (test-case "raises when invoked for sqlite"
      (check-exn
       exn:fail:user?
       (lambda () (type-declaration uuid/f 'sqlite3))))

    (test-case "dump and load"
      (define some-uuids
        '("e51f187c-6ee9-41fe-b30d-76fd9dc92225"
          "76db5072-6683-44a9-8bb0-33e582711a97"
          "fcd341f6-7731-42a1-9723-18242bdebb2b"
          "65640afa-6a5a-4f11-b373-7c2cfd970570"
          "df61bc00-7797-4a85-ac0f-23798ed44092"
          "08cfce0a-5749-420d-9944-7a37601bf22e"
          "bef42146-0060-44a1-bcf2-a7ed97cee580"
          "5ef93497-6bf4-478e-8d29-803deb4c42f6"
          "26c42415-46ac-4c42-85ac-4600c4ce0d5c"
          "0b36d520-cf66-4cfe-bc48-8b69904b9b3a"))
      (for ([initial (in-list some-uuids)])
        (check-equal? (type-load uuid/f 'postgresql initial) initial)
        (check-equal? (type-dump uuid/f 'postrgesql initial) initial))))))


(module+ test
  (require rackunit/text-ui)
  (run-tests type-tests))
