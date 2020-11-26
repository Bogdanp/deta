#lang racket/base

(require deta
         deta/private/meta
         deta/private/schema
         gregor
         racket/generic
         racket/match
         racket/port
         racket/set
         rackunit
         threading
         syntax/macro-testing
         "common.rkt")

(provide
 schema-tests)

(define-generics to-jsexpr
  (->jsexpr to-jsexpr))

(define-schema entry
  #:virtual
  ([id id/f #:primary-key #:auto-increment]
   [title string/f])

  #:methods gen:to-jsexpr
  [(define (->jsexpr e)
     (hasheq 'title (entry-title e)))])

(module schema-out-test-sub racket/base
  (require deta)
  (provide (schema-out book))

  (define-schema book
    #:virtual
    ([id id/f #:primary-key #:auto-increment]
     [title string/f]
     [author string/f]
     [(metadata #f) jsonb/f #:virtual])))

(require 'schema-out-test-sub)

(define schema-tests
  (test-suite
   "schema"

   (test-suite
    "define-schema"

    (test-case "registers schema metadata in the registry"
      (check-eq? user-schema (schema-registry-lookup 'user)))

    (test-case "raises an error if two schemas are defined with the same name"
      (check-exn
       exn:fail:user?
       (lambda _
         (define-schema user
           ([id id/f #:primary-key #:auto-increment]))

         (fail "should never get here"))))

    (test-case "defined structs can be pattern matched"
      (match (make-user #:username "bogdan")
        [(struct* user ([username u]))
         (check-equal? u "bogdan")]))

    (test-case "defined structs have an associated smart constructor"
      (check-exn
       exn:fail:contract?
       (lambda _
         (make-user #:username 1)))

      (check-true (user? (make-user #:username "bogdan@example.com"))))

    (test-case "defined structs have associated functional setters and updaters"
      (define a-user (make-user #:username "bogdan"))
      (check-equal? (~> a-user
                        (set-user-username "bogdan-paul")
                        (update-user-username string-upcase)
                        (user-username))
                    "BOGDAN-PAUL"))

    (test-case "defined structs have associated metadata"
      (define m (entity-meta (make-user #:username "bogdan")))
      (check-eq? (meta-state m) 'created)
      (check-equal? (meta-changes m) (seteq)))

    ;; XFAIL: Custom field names are not currently replaced.
    #;
    (test-case "schema fields can have custom names"
      (define q
        (with-output-to-string
          (lambda _
            (display (select (from user #:as u) u.valid?)))))

      (check-equal? q "#<query: SELECT \"u\".\"valid\" FROM \"users\" AS \"u\">"))

    (test-case "struct-options are passed to the struct definition"
      (define e (make-entry #:title "hello"))
      (check-equal? (->jsexpr e) (hasheq 'title "hello"))))

   (test-suite
    "schema-registry-lookup"

    (test-case "raises an error when given a nonexistent schema"
      (check-exn
       exn:fail?
       (lambda _
         (schema-registry-lookup 'idontexist))))

    (test-case "returns a schema given its name"
      (check-eq? (schema-registry-lookup 'user) user-schema))

    (test-case "returns a schema given itself"
      (check-eq? (schema-registry-lookup user-schema) user-schema)))

   (test-suite
    "schema-out"

    (test-case "provides all schema-related identifiers"
      (check-true (schema? book-schema))

      (define a-book
        (make-book #:title "The Lord of the Ring"
                   #:author "J.R.R. Tolkien"))

      (check-true (book? a-book))
      (check-equal? (~> (set-book-title a-book "The Lord of the Rings")
                        (update-book-title string-upcase)
                        (book-title))
                    "THE LORD OF THE RINGS")))))

(define field-tests
  (test-suite
   "fields"

   (test-suite
    "virtual"

    (test-case "raises an error on forbidden keywords on virtual fields"

      (check-exn
       exn:fail:syntax?
       (lambda _
         (convert-compile-time-error
          (define-schema illegal
            ([(metadata #f) jsonb/f #:unique #:virtual])))
         (fail "should never get here")))

      (check-exn
       exn:fail:syntax?
       (lambda _
         (convert-compile-time-error
          (define-schema illegal
            ([(metadata #f) jsonb/f #:auto-increment #:virtual])))
         (fail "should never get here")))

      (check-exn
       exn:fail:syntax?
       (lambda _
         (convert-compile-time-error
          (define-schema illegal
            ([(metadata #f) jsonb/f #:nullable #:virtual])))
         (fail "should never get here")))

      (check-exn
       exn:fail:syntax?
       (lambda _
         (convert-compile-time-error
          (define-schema illegal
            ([(metadata #f) jsonb/f #:primary-key #:virtual])))
         (fail "should never get here")))

      (check-exn
       exn:fail:syntax?
       (lambda _
         (convert-compile-time-error
          (define-schema illegal
            ([(metadata #f) jsonb/f #:name #:virtual])))
         (fail "should never get here")))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests schema-tests)
  (run-tests field-tests))
