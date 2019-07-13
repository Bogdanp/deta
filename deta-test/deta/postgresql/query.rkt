#lang racket/base

(require db
         deta
         deta/private/meta
         gregor
         racket/match
         racket/string
         rackunit
         threading)

(provide
 pg-tests)

(define current-conn
  (make-parameter #f))

(define-schema pg-user
  ([id id/f #:primary-key #:auto-increment]
   [username string/f #:unique #:wrapper string-downcase]
   [(active? #f) boolean/f]
   [password-hash string/f #:nullable]
   [(created-at (now/moment)) datetime-tz/f]
   [(updated-at (now/moment)) datetime-tz/f]))

(define pg-tests
  (test-suite
   "postgres-query"

   #:before
   (lambda ()
     (drop-table! (current-conn) 'pg-user)
     (create-table! (current-conn) 'pg-user))

   (test-suite
    "insert!"

    (test-case "persists entities"
      (define u (make-pg-user #:username "bogdan@example.com"))
      (check-eq? (meta-state (entity-meta u)) 'created)

      (define u* (car (insert! (current-conn) u)))
      (check-eq? (meta-state (entity-meta u*)) 'persisted)
      (check-not-eq? (pg-user-id u*) sql-null)

      (test-case "changing a persistent entity updates its meta state"
        (define u** (set-pg-user-username u* "jim@example.com"))
        (check-eq? (meta-state (entity-meta u**)) 'changed))))

   (test-suite
    "delete!"

    (test-case "does nothing to entities that haven't been persisted"
      (define u (make-pg-user #:username "bogdan@example.com"))
      (check-equal? (delete! (current-conn) u) null))

    (test-case "deletes persisted entities"
      (define u (make-pg-user #:username "will-delete@example.com"))
      (match-define (list u*)  (insert! (current-conn) u))
      (match-define (list u**) (delete! (current-conn) u*))
      (check-eq? (meta-state (entity-meta u**)) 'deleted)))

   (test-suite
    "query"

    (test-suite
     "from"

     (test-case "retrieves whole entities from the database"
       (define all-users
         (for/list ([u (in-rows (current-conn) (from pg-user #:as u))])
           (check-true (pg-user? u))))

       (check-true (> (length all-users) 0))))

    (test-suite
     "where"

     (test-case "restricts which entities are retrieved from the database"
       (define query
         (~> (from pg-user #:as u)
             (where u.active?)))

       (define all-active-users
         (for/list ([u (in-rows (current-conn) query)]) u))

       (check-true (null? all-active-users))

       (match-define (list active-user-jim active-user-bob)
         (insert! (current-conn)
                  (make-pg-user #:username "active-user-jim@example.com"
                                #:active? #t)
                  (make-pg-user #:username "active-user-bob@example.com"
                                #:active? #t)))

       (define all-active-users*
         (for/list ([u (in-rows (current-conn) query)]) u))

       (check-equal? (length all-active-users*) 2)

       (define all-active-users-named-bob
         (for/list ([u (in-rows (current-conn)
                                (~> query
                                    (and-where (like u.username "%bob%"))))])
           u))

       (check-equal? (length all-active-users-named-bob) 1)
       (check-equal? (pg-user-id active-user-bob)
                     (pg-user-id (car all-active-users-named-bob))))))))

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
      (run-tests pg-tests))))
