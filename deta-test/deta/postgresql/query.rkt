#lang racket/base

(require db
         deta
         gregor
         racket/string
         rackunit)

(define-schema user
  ([id id/f #:primary-key #:auto-increment]
   [username string/f #:unique #:wrapper string-downcase]
   [password-hash string/f #:nullable]
   [(created-at (now/moment)) datetime-tz/f]
   [(updated-at (now/moment)) datetime-tz/f]))

(define current-conn
  (make-parameter #f))

(define query-tests
  (test-suite
   "postgresql-query"
   #:before
   (lambda _
     (drop-table! (current-conn) 'user)
     (create-table! (current-conn) 'user))

   ))

(module+ test
  (require rackunit/text-ui)

  (define pg-database (getenv "DETA_POSTGRES_DB"))
  (define pg-username (getenv "DETA_POSTGRES_USER"))
  (define pg-password (getenv "DETA_POSTGRES_PASS"))
  (when pg-database
    (parameterize ([current-conn (postgresql-connect #:server   "127.0.0.1"
                                                     #:port     5432
                                                     #:database pg-database
                                                     #:user     pg-username
                                                     #:password pg-password)])
      (run-tests query-tests))))
