#lang racket/base

(require deta
         deta/adapter/adapter
         deta/adapter/postgresql
         deta/query/struct
         rackunit)

(provide
 sql-tests)

(define-check (check-emitted q expected)
  (define emitted (adapter-emit-query postgresql-adapter (query-stmt q)))
  (check-equal? emitted expected))

(define sql-tests
  (test-suite
   "postgresq-sql"

   (test-suite
    "select"

    (check-emitted (select 1) "SELECT 1")
    (check-emitted (select (+ 1 2)) "SELECT 1 + 2")
    (check-emitted (select #t) "SELECT TRUE")
    (check-emitted (select (not #t)) "SELECT NOT TRUE")
    (check-emitted (select (sum 1)) "SELECT SUM(1)")
    (check-emitted (select (and 1 2)) "SELECT 1 AND 2")
    (check-emitted (select (bitwise-or 1 2)) "SELECT 1 | 2")
    (check-emitted (select (concat "hello " "world!")) "SELECT 'hello ' || 'world!'")
    (check-emitted (select (is null null)) "SELECT NULL IS NULL")
    (check-emitted (select (like u.a "hello%")) "SELECT \"u\".\"a\" LIKE 'hello%'")
    (check-emitted (select (not-like u.a "hello%")) "SELECT \"u\".\"a\" NOT LIKE 'hello%'")
    (check-emitted (select (as (between (now)
                                        (- (now) (interval "7 days"))
                                        (+ (now) (interval "7 days")))
                               is_between))
                   "SELECT NOW() BETWEEN NOW() - INTERVAL '7 days' AND NOW() + INTERVAL '7 days' AS \"is_between\""))))

(module+ test
  (require rackunit/text-ui)
  (run-tests sql-tests))
