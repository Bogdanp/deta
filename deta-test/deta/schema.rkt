#lang racket/base

(require deta
         deta/private/meta
         gregor
         rackunit)

(provide
 schema-tests)

(define-schema user
  ([id id/f #:primary-key #:auto-increment]
   [username string/f #:unique]
   [password-hash string/f #:nullable]
   [(created-at (now/moment)) datetime-tz/f]
   [(updated-at (now/moment)) datetime-tz/f]))

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
