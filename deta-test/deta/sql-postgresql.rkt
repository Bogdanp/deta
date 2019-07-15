#lang racket/base

(require deta
         deta/private/adapter/adapter
         deta/private/adapter/postgresql
         (only-in deta/private/query query-stmt)
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

    (check-emitted (~> (update "users"
                               #:as u
                               #:set ([username ,1]
                                      [password-hash ,2])))
                   "UPDATE \"users\" AS \"u\" SET \"username\" = $1, \"password_hash\" = $2")

    (check-emitted (~> (update "users"
                               #:as u
                               #:set ([username ,1]
                                      [password-hash ,2]))
                       (where (= u.id ,3)))
                   "UPDATE \"users\" AS \"u\" SET \"username\" = $1, \"password_hash\" = $2 WHERE \"u\".\"id\" = $3")

    (check-emitted (~> (update "users"
                               #:as u
                               #:set ([username ,1]
                                      [password-hash ,2]))
                       (where (= u.id ,3))
                       (or-where (= u.id ,4)))
                   "UPDATE \"users\" AS \"u\" SET \"username\" = $1, \"password_hash\" = $2 WHERE (\"u\".\"id\" = $3) OR (\"u\".\"id\" = $4)"))

   (test-suite
    "select"

    (check-emitted (select 1)
                   "SELECT 1")

    (check-emitted (select 1.5)
                   "SELECT 1.5")

    (check-emitted (select (+ 1 2))
                   "SELECT 1 + 2")

    (check-emitted (select (* 3 (+ 1 2)))
                   "SELECT 3 * (1 + 2)")

    (check-emitted (select (abs -1))
                   "SELECT ABS(-1)")

    (check-emitted (select (random))
                   "SELECT RANDOM()")

    (check-emitted (select #t)
                   "SELECT TRUE")

    (check-emitted (select #f)
                   "SELECT FALSE")

    (check-emitted (select (and 1 2))
                   "SELECT 1 AND 2")

    (check-emitted (select (or #t #f))
                   "SELECT TRUE OR FALSE")

    (check-emitted (select (not #t))
                   "SELECT NOT TRUE")

    (check-emitted (select (not (or #t #f)))
                   "SELECT NOT (TRUE OR FALSE)")

    (check-emitted (select (or (and (not #t) #f) 1))
                   "SELECT ((NOT TRUE) AND FALSE) OR 1")

    (check-emitted (select (sum 1))
                   "SELECT SUM(1)")

    (check-emitted (select (bitwise-or 1 2))
                   "SELECT 1 | 2")

    (check-emitted (select (concat "hello " "world!"))
                   "SELECT CONCAT('hello ', 'world!')")

    (check-emitted (select "quoting 'test'")
                   "SELECT 'quoting ''test'''")

    (check-emitted (select (position "om" "Thomas"))
                   "SELECT POSITION('om' IN 'Thomas')")

    (check-emitted (select (trim leading "x" "xxtestxx"))
                   "SELECT TRIM(LEADING 'x' FROM 'xxtestxx')")

    (check-emitted (select (similar-to "x" "%x%"))
                   "SELECT 'x' SIMILAR TO '%x%'")

    (check-emitted (select (~ "x" "^x$"))
                   "SELECT 'x' ~ '^x$'")

    (check-emitted (select (~* "x" "^x$"))
                   "SELECT 'x' ~* '^x$'")

    (check-emitted (select (!~ "x" "^x$"))
                   "SELECT 'x' !~ '^x$'")

    (check-emitted (select (!~* "x" "^x$"))
                   "SELECT 'x' !~* '^x$'")

    (check-emitted (select (is null null))
                   "SELECT NULL IS NULL")

    (check-emitted (select (like u.a "hello%"))
                   "SELECT \"u\".\"a\" LIKE 'hello%'")

    (check-emitted (select (not-like u.a "hello%"))
                   "SELECT \"u\".\"a\" NOT LIKE 'hello%'")

    (check-emitted (select (in 1 '(1 2 3)))
                   "SELECT 1 IN (1, 2, 3)")

    (check-emitted (select (in 1 (list 1 2 3)))
                   "SELECT 1 IN (1, 2, 3)")

    (check-emitted (select (not-in 1 '(1 2 3)))
                   "SELECT 1 NOT IN (1, 2, 3)")

    (check-emitted (select (not-in 1 (list 1 2 3)))
                   "SELECT 1 NOT IN (1, 2, 3)")

    (check-emitted (select (date "1950-01-01"))
                   "SELECT DATE '1950-01-01'")

    (check-emitted (select (interval "7 days"))
                   "SELECT INTERVAL '7 days'")

    (check-emitted (select (isfinite (interval "7 days")))
                   "SELECT ISFINITE(INTERVAL '7 days')")

    (check-emitted (select (time "12:30"))
                   "SELECT TIME '12:30'")

    (check-emitted (select (timestamp "1950-01-01 00:00:00"))
                   "SELECT TIMESTAMP '1950-01-01 00:00:00'")

    (check-emitted (select (date_part "year" (date "1950-01-01")))
                   "SELECT DATE_PART('year', DATE '1950-01-01')")

    (check-emitted (select (extract year (timestamp "1950-01-01 00:00:00")))
                   "SELECT EXTRACT(YEAR FROM (TIMESTAMP '1950-01-01 00:00:00'))")

    (check-emitted (select (cast "1950-01-01" date))
                   "SELECT CAST('1950-01-01' AS DATE)")

    (check-emitted (select (as (between (now)
                                        (- (now) (interval "7 days"))
                                        (+ (now) (interval "7 days")))
                               is_between))
                   (~a "SELECT ((NOW()) "
                       "BETWEEN ((NOW()) - (INTERVAL '7 days')) "
                       "AND ((NOW()) + (INTERVAL '7 days'))) "
                       "AS \"is_between\""))

    (check-emitted (select
                    (from "departments" #:as d)
                    (case
                      [(> (min d.employees) 0)
                       (avg (/ d.expenses d.employees))]))
                   "SELECT CASE WHEN (MIN(\"d\".\"employees\")) > 0 THEN AVG(\"d\".\"expenses\" / \"d\".\"employees\") END FROM \"departments\" AS \"d\"")

    (check-emitted (select
                    (from "departments" #:as d)
                    (case
                      [(> (min d.employees) 0)
                       (avg (/ d.expenses d.employees))]
                      [else 0]))
                   "SELECT CASE WHEN (MIN(\"d\".\"employees\")) > 0 THEN AVG(\"d\".\"expenses\" / \"d\".\"employees\") ELSE 0 END FROM \"departments\" AS \"d\"")

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
     "limit"

     (check-emitted (~> (from "books" #:as b)
                        (limit 20))
                    "SELECT * FROM \"books\" AS \"b\" LIMIT 20")

     (check-emitted (~> (from "books" #:as b)
                        (offset 10)
                        (limit 20))
                    "SELECT * FROM \"books\" AS \"b\" LIMIT 20 OFFSET 10"))

    (test-suite
     "placeholders"

     (check-emitted/placeholders (select ,42) "SELECT $1" '(42))
     (let ([x 1]
           [y "hello"])
       (check-emitted/placeholders (select (<> ,x ,y))
                                   "SELECT $1 <> $2"
                                   '(1 "hello")))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests sql-tests))
