#lang racket/base

(require deta
         deta/private/meta
         rackunit)

(provide
 schema-tests)

(define-schema user
  ([id id/f #:primary-key]
   [username string/f]
   [password-hash string/f #:nullable]))

(define schema-tests
  (test-suite
   "schema"

   (test-suite
    "define-schema"

    (test-case "registers schema metadata in the registry"
      (check-eq? user-schema (schema-registry-ref 'user)))

    (test-case "defined structs have an associated smart constructor"
      (check-exn
       exn:fail:contract?
       (lambda _
         (make-user #:username 1)))

      (check-true (user? (make-user #:username "bogdan@example.com"))))

    (test-case "defined structs have associated functional setters and updaters"
      (define a-user (make-user #:username "bogdan"))
      (check-equal? (user-username (update-user-username a-user string-upcase)) "BOGDAN"))

    (test-case "defined structs have associated metadata"
      (check-eq? (meta-state (entity-meta (make-user #:username "bogdan"))) 'created)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests schema-tests))
