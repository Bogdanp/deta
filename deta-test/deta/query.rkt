#lang racket/base

(require db
         deta
         deta/private/meta
         racket/string
         rackunit)

(provide
 query-tests)

(define current-conn
  (make-parameter #f))

(define-schema user
  ([id id/f #:primary-key #:auto-increment]
   [username string/f #:wrapper string-downcase]
   [password-hash string/f #:nullable]))

(define query-tests
  (test-suite
   "query"

   (test-suite
    "insert!"

    (test-case "persists entities"
      (define u (make-user #:username "bogdan@example.com"))
      (check-eq? (meta-state (entity-meta u)) 'created)

      (define u* (car (insert! (current-conn) u)))
      (check-eq? (meta-state (entity-meta u*)) 'persisted)
      (check-not-eq? (user-id u*) sql-null)))))

(module+ test
  (require rackunit/text-ui)

  (parameterize ([current-conn (sqlite3-connect #:database 'memory)])
    (create-table! (current-conn) user-schema)
    (run-tests query-tests))

  (define pg-database (getenv "DETA_POSTGRES_DB"))
  (define pg-username (getenv "DETA_POSTGRES_USER"))
  (define pg-password (getenv "DETA_POSTGRES_PASS"))
  (when pg-database
    (parameterize ([current-conn (postgresql-connect #:server   "127.0.0.1"
                                                     #:port     5432
                                                     #:database pg-database
                                                     #:user     pg-username
                                                     #:password pg-password)])
      (create-table! (current-conn) user-schema)
      (run-tests query-tests))))
