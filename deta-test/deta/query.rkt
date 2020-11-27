#lang racket/base

(require db
         deta
         deta/private/meta
         gregor
         racket/match
         racket/port
         racket/set
         rackunit
         threading
         "common.rkt")

(provide
 query-tests)

(define current-conn
  (make-parameter #f))

(define (current-conn-postgres?)
  (eq? (dbsystem-name (connection-dbsystem (current-conn))) 'postgresql))

(define query-tests
  (test-suite
   "query"

   #:before
   (lambda ()
     (drop-table! (current-conn) 'book-with-nulls)
     (drop-table! (current-conn) 'user)
     (drop-table! (current-conn) 'password-reset)
     (drop-table! (current-conn) 'hybrid)
     (create-table! (current-conn) 'book-with-nulls)
     (create-table! (current-conn) 'user)
     (create-table! (current-conn) 'password-reset)
     (create-table! (current-conn) 'hybrid))

   (test-suite
    "prop:statement"

    (test-case "queries can be executed by the standard query functions"
      (check-equal? (query-value (current-conn) (select _ 1)) 1))

    (test-case "queries with placeholders can be executed by the standard query functions"
      (check-equal? (query-value (current-conn) (select _ (cast ,1 int))) 1)))

   (test-suite
    "prop:custom-write"

    (test-case "prints queries"
      (check-equal?
       (with-output-to-string
         (lambda _
           (display (select _ 1))))
       "#<query: SELECT 1>")

      (check-equal?
       (with-output-to-string
         (lambda _
           (display (select _ ,1))))
       "#<query: SELECT $1 1>")))

   (test-suite
    "create-table!"

    (test-case "fails with a contract error if given an unknown schema id"
      (check-exn
       (lambda (e)
         (and (exn:fail:contract? e)
              (check-regexp-match "unregistered schema" (exn-message e))))
       (lambda _
         (create-table! (current-conn) 'idontexist))))

    (test-case "ignores virtual fields"
      (define the-hybrid (make-hybrid #:slug "hello-world" #:comment "some value"))
      (insert-one! (current-conn) the-hybrid)
      (define row (query-row (current-conn) "SELECT * FROM hybrids"))
      (check-match row (vector (? number?) "hello-world"))))

   (test-suite
    "insert!"

    (test-case "persists entities"
      (define u (make-user #:username "bogdan@example.com"))
      (check-eq? (meta-state (entity-meta u)) 'created)

      (define u* (car (insert! (current-conn) u)))
      (check-eq? (meta-state (entity-meta u*)) 'persisted)
      (check-not-eq? (user-id u*) sql-null)

      (test-case "changing a persistent entity updates its meta state"
        (define u** (set-user-username u* "jim@example.com"))
        (check-eq? (meta-state (entity-meta u**)) 'changed)))

    (test-case "does not persist entities with virtual schemas"
      (define-schema v
        #:virtual
        ([x integer/f]))

      (check-exn
       exn:fail:user?
       (lambda _
         (insert! (current-conn) (make-v #:x 42)))))

    (test-case "persists entities w/o a primary key"
      (check-not-false
       (insert-one! (current-conn) (make-password-reset #:user-id 1))))

    (test-case "persists entities containing nulls"
      (check-not-false
       (insert-one! (current-conn) (make-book-with-nulls #:title "Euclid"))))

    (test-case "persists entities containing datetime fields"
      (check-not-false
       (insert-one! (current-conn) (make-book-with-nulls #:title "Euclid"
                                                         #:published-on (now))))))

   (test-suite
    "update!"

    (test-case "does nothing to entities that haven't been persisted"
      (define u (make-user #:username "bogdan@example.com"))
      (check-equal? (update! (current-conn) u) null))

    (test-case "does nothing to entities with only virtual fields changed"
      (define the-hybrid (make-hybrid #:slug "eureka" #:comment "some value"))
      (define changed-hybrid
        (~> (insert-one! (current-conn) the-hybrid)
            (set-hybrid-comment "some other value")))
      (check-false (update-one! (current-conn) changed-hybrid)))

    (test-case "does nothing to entities that haven't been changed"
      (match-define (list u)
        (insert! (current-conn)
                 (make-user #:username "bogdan-for-noop-update@example.com")))

      (check-equal? (meta-changes (entity-meta u)) (seteq))

      (define u* (update-one! (current-conn) u))
      (check-false u*))

    (test-case "updates entities that have been changed"
      (match-define (list u)
        (insert! (current-conn)
                 (make-user #:username "bogdan-for-update@example.com")))

      (define u* (set-user-username u "bogdan-for-update-changed@example.com"))
      (check-eq? (meta-state (entity-meta u*)) 'changed)
      (check-equal? (meta-changes (entity-meta u*)) (seteq 'username))

      (match-define (list u**) (update! (current-conn) u*))
      (check-eq? (meta-state (entity-meta u**)) 'persisted)
      (check-equal? (meta-changes (entity-meta u**)) (seteq)))

    (test-case "updates entities containing null values"
      (define book
        (insert-one! (current-conn) (make-book-with-nulls #:title "Euclid")))

      (check-not-false
       (update-one! (current-conn) (set-book-with-nulls-title book "Euclid's Excellent Adventure"))))

    (test-case "runs pre-persist hooks"
      (define u
        (insert-one! (current-conn)
                     (make-user #:username "bogdan-for-hooks@example.com")))

      (sync (system-idle-evt))
      (match-define (list u*)
        (update! (current-conn)
                 (update-user-username u values)))

      (check-not-equal? (user-updated-at u)
                        (user-updated-at u*))))

   (test-suite
    "delete!"

    (test-case "does nothing to entities that haven't been persisted"
      (define u (make-user #:username "bogdan@example.com"))
      (check-equal? (delete! (current-conn) u) null))

    (test-case "deletes persisted entities"
      (define u (make-user #:username "will-delete@example.com"))
      (match-define (list u*)  (insert! (current-conn) u))
      (match-define (list u**) (delete! (current-conn) u*))
      (check-eq? (meta-state (entity-meta u**)) 'deleted))

    (test-case "runs pre-delete hooks"
      (define u
        (insert-one! (current-conn)
                     (make-user #:username "will-delete-for-hooks@example.com")))

      (delete! (current-conn) u)
      (check-not-false (member (user-id u) (deleted-users)))))

   (test-suite
    "query"

    (test-suite
     "select"

     (test-case "can retrieve arbitrary data"
       (define x
         (for/first ([(x) (in-entities (current-conn) (select _ 1))])
           x))

       (check-equal? x 1))

     (test-case "can retrieve data containing nulls"
       (define book
         (insert-one! (current-conn) (make-book-with-nulls #:title "Euclid")))

       (check-equal? (book-with-nulls-published-on book)
                     (book-with-nulls-published-on
                      (lookup (current-conn) (~> (from book-with-nulls #:as b)
                                                 (where (= b.id ,(book-with-nulls-id book))))))))

     (test-case "can retrieve data containing datetime fields without tz"
       (define book
         (insert-one! (current-conn) (make-book-with-nulls #:title "Euclid"
                                                           #:published-on (now))))

       (check-true (= (seconds-between
                       (book-with-nulls-published-on book)
                       (book-with-nulls-published-on
                        (lookup (current-conn) (~> (from book-with-nulls #:as b)
                                                   (where (= b.id ,(book-with-nulls-id book)))))))
                      0)))

     (test-case "can retrieve subsets of data from schemas"
       (define usernames
         (for/list ([(username) (in-entities (current-conn)
                                             (~> (from user #:as u)
                                                 (select u.username)))])
           username))

       (check-true (not (null? usernames))))

     (test-case "can project query results onto virtual schemas"
       (define-schema res
         #:virtual
         ([x integer/f]
          [y string/f]))

       (define r
         (for/first ([r (in-entities (current-conn)
                                     (~> (select _ 1 "hello")
                                         (project-onto res-schema)))])
           r))

       (check-true (res? r))
       (check-equal? (res-x r) 1)
       (check-equal? (res-y r) "hello"))

     (test-case "can retrieve array data"
       (when (current-conn-postgres?)
         (define-schema res
           #:virtual
           ([roles (array/f string/f)]))

         (define r
           (for/first ([r (in-entities (current-conn)
                                       (~> (select _ (as (array "a" "b" "c") roles))
                                           (project-onto res-schema)))])
             r))

         (check-true (res? r))
         (check-equal? (res-roles r) #("a" "b" "c"))))

     (test-suite
      "from"

      (test-case "retrieves whole entities from the database"
        (define all-users
          (for/list ([u (in-entities (current-conn) (from user #:as u))])
            (check-equal? (meta-state (entity-meta u)) 'persisted)
            (check-true (user? u))))

        (check-true (> (length all-users) 0))))

     (test-case "retrieves whole entities containing virtual fields from the database"
       (drop-table! (current-conn) 'hybrid)
       (create-table! (current-conn) 'hybrid)
       (insert! (current-conn)
                (make-hybrid #:slug "hybrid-0" #:comment "some value")
                (make-hybrid #:slug "hybrid-1" #:comment "some other value"))
       (define default-comment (hybrid-comment (make-hybrid #:slug "n/a")))
       (for ([h (in-entities (current-conn) (~> (from hybrid #:as h) (order-by ([slug]))))]
             [slug (in-list '("hybrid-0" "hybrid-1"))])
         (check-equal? (hybrid-slug h) slug)
         (check-equal? (hybrid-comment h) default-comment)))

     (test-suite
      "where"

      (test-case "restricts which entities are retrieved from the database"
        (define query
          (~> (from user #:as u)
              (where u.active?)))

        (define all-active-users
          (for/list ([u (in-entities (current-conn) query)]) u))

        (check-true (null? all-active-users))

        (match-define (list active-user-jim active-user-bob)
          (insert! (current-conn)
                   (make-user #:username "active-user-jim@example.com"
                              #:active? #t)
                   (make-user #:username "active-user-bob@example.com"
                              #:active? #t)))

        (define all-active-users*
          (for/list ([u (in-entities (current-conn) query)]) u))

        (check-equal? (length all-active-users*) 2)

        (define all-active-users-named-bob
          (for/list ([u (in-entities (current-conn)
                                     (~> query
                                         (where (like u.username "%bob%"))))])
            u))

        (check-equal? (length all-active-users-named-bob) 1)
        (check-equal? (user-id active-user-bob)
                      (user-id (car all-active-users-named-bob))))

      (test-case "can handle boolean values within arbitrary queries"
        (check-not-exn
         (lambda ()
           (for ([v '(#t #f)])
             (query-exec (current-conn)
                         (~> (from user #:as u)
                             (select u.id)
                             (where (= u.active? ,v))))))))))

    (test-suite
     "update"

     (test-case "can update arbitrary tables"
       (query-exec (current-conn)
                   (~> (from user #:as u)
                       (update [active? #t])))

       (for ([u (in-entities (current-conn) (from user #:as u))])
         (check-true (user-active? u)))))

    (test-suite
     "delete"

     (test-case "can delete arbitrary data"
       (define q
         (~> (from user #:as u)
             (where u.active?)))

       (query-exec (current-conn) (delete q))
       (check-equal? (query-value (current-conn)
                                  (select q (count *))) 0)))

    (test-suite
     "lookup"

     (test-case "can look up individual values"
       (check-equal? (lookup (current-conn) (select _ 1)) 1))

     (test-case "can look up tuples of values"
       (define-values (a b)
         (lookup (current-conn) (select _ 1 2)))

       (check-equal? a 1)
       (check-equal? b 2))

     (test-case "can look up entities"
       (define a-user
         (insert-one! (current-conn)
                      (make-user #:username "user-for-lookup@example.com")))

       (define user
         (lookup (current-conn) (~> (from user #:as u)
                                    (where (= u.id ,(user-id a-user))))))

       (check-true (user? user)))

     (test-case "can fail to look up entities"
       (check-false (lookup (current-conn)
                            (~> (from user #:as u)
                                (where #f)))))))))

(module+ test
  (require rackunit/text-ui)

  (let ()
    (define (connect)
      (sqlite3-connect #:database 'memory))

    (parameterize ([current-conn (connect)])
      (run-tests
       (test-suite
        "sqlite (simple conn)"

        query-tests)))

    (parameterize ([current-conn (virtual-connection connect)])
      (run-tests
       (test-suite
        "sqlite (virtual conn)"

        query-tests)))

    (let ([pool (connection-pool connect)])
      (parameterize ([current-conn (connection-pool-lease pool)])
        (run-tests
         (test-suite
          "sqlite (connection pool)"

          query-tests)))))

  (let ()
    (define pg-database (getenv "DETA_POSTGRES_DB"))
    (define pg-username (getenv "DETA_POSTGRES_USER"))
    (define pg-password (getenv "DETA_POSTGRES_PASS"))
    (when pg-database
      (define (connect)
        (postgresql-connect #:server   "127.0.0.1"
                            #:port     5432
                            #:database pg-database
                            #:user     pg-username
                            #:password pg-password))

      (parameterize ([current-conn (connect)])
        (run-tests
         (test-suite
          "postgresql (simple conn)"

          query-tests)))

      (parameterize ([current-conn (virtual-connection connect)])
        (run-tests
         (test-suite
          "postgresql (virtual conn)"

          query-tests)))

      (let ([pool (connection-pool connect)])
        (parameterize ([current-conn (connection-pool-lease pool)])
          (run-tests
           (test-suite
            "postgresql (connection pool)"

            query-tests)))))))
