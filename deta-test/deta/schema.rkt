#lang racket/base

(require deta
         deta/private/meta
         gregor
         racket/match
         racket/set
         rackunit
         threading
         "common.rkt")

(provide
 schema-tests)

(define schema-tests
  (test-suite
   "schema"

   (test-suite
    "define-schema"

    (test-case "registers schema metadata in the registry"
      (check-eq? user-schema (schema-registry-ref 'user)))

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
      (check-equal? (meta-changes m) (seteq))))

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
      (check-eq? (schema-registry-lookup user-schema) user-schema)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests schema-tests))
