#lang racket/base

(require deta
         deta/adapter/adapter
         deta/adapter/postgresql
         deta/query/struct
         (prefix-in ast: deta/private/ast)
         racket/format
         rackunit
         threading)

(provide
 sql-tests)

(define (query->stmt q)
  (cond
    [(query? q) (query-stmt q)]
    [else q]))

(define-check (check-emitted q expected)
  (define-values (query _)
    (adapter-emit-query postgresql-adapter (query->stmt q)))

  (check-equal? query expected))

(define-check (check-emitted/placeholders q expected-query expected-placeholders)
  (define-values (query args)
    (adapter-emit-query postgresql-adapter (query->stmt q)))

  (check-equal? query expected-query)
  (check-equal? args expected-placeholders))

(define sql-tests
  (test-suite
   "postgresq-sql"

   (test-suite
    "update"

    (check-emitted (ast:update (ast:table "users")
                               (ast:assignments
                                (list (cons (ast:column "username")
                                            (ast:placeholder 1))
                                      (cons (ast:column "password_hash")
                                            (ast:placeholder 2))))
                               (ast:where (ast:app (ast:name '=)
                                                   (list (ast:column "id")
                                                         (ast:placeholder 3)))))
                   "UPDATE \"users\" SET \"username\" = $1, \"password_hash\" = $2 WHERE \"id\" = $3"))

   (test-suite
    "select"

    (check-emitted (select 1) "SELECT 1")
    (check-emitted (select 1.5) "SELECT 1.5")
    (check-emitted (select (+ 1 2)) "SELECT 1 + 2")
    (check-emitted (select #t) "SELECT TRUE")
    (check-emitted (select #f) "SELECT FALSE")
    (check-emitted (select (and 1 2)) "SELECT 1 AND 2")
    (check-emitted (select (or #t #f)) "SELECT TRUE OR FALSE")
    (check-emitted (select (not #t)) "SELECT NOT TRUE")
    (check-emitted (select (not (or #t #f))) "SELECT NOT (TRUE OR FALSE)")
    (check-emitted (select (or (and (not #t) #f) 1)) "SELECT ((NOT TRUE) AND FALSE) OR 1")
    (check-emitted (select (sum 1)) "SELECT SUM(1)")
    (check-emitted (select (bitwise-or 1 2)) "SELECT 1 | 2")
    (check-emitted (select (concat "hello " "world!")) "SELECT 'hello ' || 'world!'")
    (check-emitted (select (is null null)) "SELECT NULL IS NULL")
    (check-emitted (select (like u.a "hello%")) "SELECT \"u\".\"a\" LIKE 'hello%'")
    (check-emitted (select (not-like u.a "hello%")) "SELECT \"u\".\"a\" NOT LIKE 'hello%'")
    (check-emitted (select (as (between (now)
                                        (- (now) (interval "7 days"))
                                        (+ (now) (interval "7 days")))
                               is_between))
                   (~a "SELECT ((NOW()) "
                       "BETWEEN ((NOW()) - (INTERVAL '7 days')) "
                       "AND ((NOW()) + (INTERVAL '7 days'))) "
                       "AS \"is_between\"")))

   (test-suite
    "group-by"

    (check-emitted (~> (from "books" #:as b)
                       (select b.year (count b.title))
                       (group-by b.year))
                   "SELECT \"b\".\"year\", COUNT(\"b\".\"title\") FROM \"books\" AS \"b\" GROUP BY \"b\".\"year\""))

   (test-suite
    "order-by"

    (check-emitted (~> (from "books" #:as b)
                       (select b.title)
                       (order-by ([b.year])))
                   "SELECT \"b\".\"title\" FROM \"books\" AS \"b\" ORDER BY \"b\".\"year\"")
    (check-emitted (~> (from "books" #:as b)
                       (select b.title)
                       (order-by ([b.year #:desc]
                                  [b.title])))
                   "SELECT \"b\".\"title\" FROM \"books\" AS \"b\" ORDER BY \"b\".\"year\" DESC, \"b\".\"title\"")
    (check-emitted (~> (from "books" #:as b)
                       (select b.title)
                       (order-by ([b.year #:desc]
                                  [b.title #:asc])))
                   "SELECT \"b\".\"title\" FROM \"books\" AS \"b\" ORDER BY \"b\".\"year\" DESC, \"b\".\"title\""))

   (test-suite
    "offset"

    (check-emitted (~> (from "books" #:as b)
                       (select b.title)
                       (offset 20)
                       (order-by ([b.title])))
                   "SELECT \"b\".\"title\" FROM \"books\" AS \"b\" ORDER BY \"b\".\"title\" OFFSET 20"))

   (test-suite
    "fetch"

    (check-emitted (~> (from "books" #:as b)
                       (fetch 20))
                   "SELECT * FROM \"books\" AS \"b\" FETCH NEXT 20 ROWS")

    (check-emitted (~> (from "books" #:as b)
                       (offset 10)
                       (fetch 20))
                   "SELECT * FROM \"books\" AS \"b\" OFFSET 10 FETCH NEXT 20 ROWS"))

   (test-suite
    "placeholders"

    (check-emitted/placeholders (select ,42) "SELECT $1" '(42))
    (let ([x 1]
          [y "hello"])
      (check-emitted/placeholders (select (<> ,x ,y))
                                  "SELECT $1 <> $2"
                                  '(1 "hello"))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests sql-tests))
