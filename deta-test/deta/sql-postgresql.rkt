#lang at-exp racket/base

(require deta
         (prefix-in ast: deta/private/ast)
         deta/private/dialect/dialect
         deta/private/dialect/postgresql
         (only-in deta/private/query
                  query-stmt)
         racket/format
         rackunit
         threading)

(provide
 sql-tests)

(define (query->stmt q)
  (if (query? q)
      (query-stmt q)
      q))

(define (emit q)
  (define-values (query _)
    (dialect-emit-query postgresql-dialect (query->stmt q)))
  query)

(define-check (check-emitted q expected)
  (define query (emit q))
  (with-check-info
    (['query query]
     ['expected expected])
    (unless (equal? query expected)
      (fail-check))))

(define-check (check-emitted/placeholders q expected-query expected-placeholders)
  (define-values (query args)
    (dialect-emit-query postgresql-dialect (query->stmt q)))

  (with-check-info
    (['query query]
     ['expected expected-query])
    (unless (equal? query expected-query)
      (fail-check)))

  (with-check-info
    (['placeholders args]
     ['expected expected-placeholders])
    (unless (equal? args expected-placeholders)
      (fail-check))))

(define sql-tests
  (test-suite
   "postgresq-sql"

   (test-suite
    "select"

    (check-emitted (select _ 1)
                   "SELECT 1")

    (check-emitted (select _ #:distinct 1)
                   "SELECT DISTINCT 1")

    (check-emitted (select _ 1.5)
                   "SELECT 1.5")

    (check-emitted (select _ (+ 1 2))
                   "SELECT 1 + 2")

    (check-emitted (select _ (* 3 (+ 1 2)))
                   "SELECT 3 * (1 + 2)")

    (check-emitted (select _ (* 3 (+ 1 2) (* 2 3)))
                   "SELECT 3 * (1 + 2) * (2 * 3)")

    (check-emitted (select _ (abs -1))
                   "SELECT ABS(-1)")

    (check-emitted (select _ (random))
                   "SELECT RANDOM()")

    (check-emitted (select _ #t)
                   "SELECT TRUE")

    (check-emitted (select _ #f)
                   "SELECT FALSE")

    (check-emitted (select _ u.UserName)
                   @~a{SELECT u."UserName"})

    (check-emitted (select _ u.user_name)
                   "SELECT u.user_name")

    (check-emitted (select _ (and 1))
                   "SELECT 1")

    (check-emitted (select _ (and 1 2))
                   "SELECT 1 AND 2")

    (check-emitted (select _ (and 1 2 3))
                   "SELECT 1 AND 2 AND 3")

    (check-emitted (select _ (and 1 2 3 4))
                   "SELECT 1 AND 2 AND 3 AND 4")

    (check-emitted (select _ (or #t #f))
                   "SELECT TRUE OR FALSE")

    (check-emitted (select _ (or #t #f #f))
                   "SELECT TRUE OR FALSE OR FALSE")

    (check-emitted (select _ (or #t (+ 1 2 3) #f))
                   "SELECT TRUE OR (1 + 2 + 3) OR FALSE")

    (check-emitted (select _ (not #t))
                   "SELECT NOT TRUE")

    (check-emitted (select _ (not (or #t #f)))
                   "SELECT NOT (TRUE OR FALSE)")

    (check-emitted (select _ (or (and (not #t) #f) 1))
                   "SELECT ((NOT TRUE) AND FALSE) OR 1")

    (check-emitted (select _ (sum 1))
                   "SELECT SUM(1)")

    (check-emitted (select _ (bitwise-not 1))
                   "SELECT ~ 1")

    (check-emitted (select _ (bitwise-and 1 2))
                   "SELECT 1 & 2")

    (check-emitted (select _ (bitwise-or 1 2))
                   "SELECT 1 | 2")

    (check-emitted (select _ (bitwise-xor 1 2))
                   "SELECT 1 # 2")

    (check-emitted (select _ (concat "hello " "world!"))
                   "SELECT CONCAT('hello ', 'world!')")

    (check-emitted (select _ "quoting 'test'")
                   "SELECT 'quoting ''test'''")

    (check-emitted (select _ (position "om" "Thomas"))
                   "SELECT POSITION('om' IN 'Thomas')")

    (check-emitted (select _ (trim leading "x" "xxtestxx"))
                   "SELECT TRIM(LEADING 'x' FROM 'xxtestxx')")

    (check-emitted (select _ (similar-to "x" "%x%"))
                   "SELECT 'x' SIMILAR TO '%x%'")

    (check-emitted (select _ (is null null))
                   "SELECT NULL IS NULL")

    (check-emitted (select _ (like u.a "hello%"))
                   "SELECT u.a LIKE 'hello%'")

    (check-emitted (select _ (ilike u.a "hello%"))
                   "SELECT u.a ILIKE 'hello%'")

    (check-emitted (select _ (in 1 (list 1 2 3)))
                   "SELECT 1 IN (1, 2, 3)")

    (check-emitted (select _ (in 1 '(1 2 3)))
                   "SELECT 1 IN (1, 2, 3)")

    (check-emitted (select _ (array 1 2 3))
                   "SELECT ARRAY[1, 2, 3]")

    (check-emitted (select _ (array-concat (array "a") (array "b" "c")))
                   "SELECT ARRAY['a'] || ARRAY['b', 'c']")

    (check-emitted (select _ (array-contains? (array 1 2) (array 1)))
                   "SELECT ARRAY[1, 2] @> ARRAY[1]")

    (check-emitted (select _ (array-overlap? (array "a") (array "b" "c")))
                   "SELECT ARRAY['a'] && ARRAY['b', 'c']")

    (check-emitted (select _ (array-ref (array 1 2 3) 2))
                   "SELECT (ARRAY[1, 2, 3])[2]")

    (check-emitted (select _ (array-slice (array 1 2 3) 2 5))
                   "SELECT (ARRAY[1, 2, 3])[2:5]")

    (check-emitted (select _ (array (array "a")
                                    (array "b")
                                    (array "c")))
                   "SELECT ARRAY[ARRAY['a'], ARRAY['b'], ARRAY['c']]")

    (check-emitted (select _ (= u.username (any (array "a" "b" "c"))))
                   "SELECT u.username = ANY(ARRAY['a', 'b', 'c'])")

    (check-emitted (select _ (= u.username (any ,(list "a" "b" "c"))))
                   "SELECT u.username = ANY($1)")

    (check-emitted (select _ (date "1950-01-01"))
                   "SELECT DATE '1950-01-01'")

    (check-emitted (select _ (interval "7 days"))
                   "SELECT INTERVAL '7 days'")

    (check-emitted (select _ (isfinite (interval "7 days")))
                   "SELECT ISFINITE(INTERVAL '7 days')")

    (check-emitted (select _ (time "12:30"))
                   "SELECT TIME '12:30'")

    (check-emitted (select _ (timestamp "1950-01-01 00:00:00"))
                   "SELECT TIMESTAMP '1950-01-01 00:00:00'")

    (check-emitted (select _ (date_part "year" (date "1950-01-01")))
                   "SELECT DATE_PART('year', DATE '1950-01-01')")

    (check-emitted (select _ (extract year (timestamp "1950-01-01 00:00:00")))
                   "SELECT EXTRACT(YEAR FROM (TIMESTAMP '1950-01-01 00:00:00'))")

    (check-emitted (select _ (cast "1950-01-01" date))
                   "SELECT CAST('1950-01-01' AS DATE)")

    (check-emitted
     (select _ (as (between (now)
                            (- (now) (interval "7 days"))
                            (+ (now) (interval "7 days")))
                   is_between))

     "SELECT ((NOW()) BETWEEN ((NOW()) - (INTERVAL '7 days')) AND ((NOW()) + (INTERVAL '7 days'))) AS is_between")

    (test-case "fails when known operators are called with bad arities"
      (check-exn
       #rx"not: arity mismatch"
       (lambda ()
         (emit (select _ (not)))))
      (check-exn
       #rx"json-subset.: arity mismatch"
       (lambda ()
         (emit (select _ (json-subset? "{}" "{}" "{}"))))))

    (test-case "supports DISTINCT in from-queries"
      (check-emitted
       (~> (from "tags" #:as t)
           (select #:distinct t.name t.count))
       "SELECT DISTINCT t.name, t.\"count\" FROM tags AS t"))

    (test-case "supports COND as an alias for CASE"
      (check-emitted
       (select
        (from "departments" #:as d)
        (cond
          [(> (min d.employees) 0)
           (avg (/ d.expenses d.employees))]))

       "SELECT CASE WHEN (MIN(d.employees)) > 0 THEN AVG(d.expenses / d.employees) END FROM departments AS d"))

    (test-case "COND expressions support ELSE clauses"
      (check-emitted
       (select
        (from "departments" #:as d)
        (cond
          [(> (min d.employees) 0)
           (avg (/ d.expenses d.employees))]
          [else 0]))

       "SELECT CASE WHEN (MIN(d.employees)) > 0 THEN AVG(d.expenses / d.employees) ELSE 0 END FROM departments AS d"))

    (test-suite
     "from"

     (test-case "supports runtime table names"
       (let ([tbl "example"])
         (check-emitted
          (~> (from ,tbl #:as t)
              (select t.x))

          "SELECT t.x FROM example AS t")))

     (test-suite
      "subquery"

      (test-case "emits subqueries in select clauses"
        (check-emitted
         (select
          _
          (as (subquery (select _ 1)) a)
          (as (subquery (select (from "b" #:as b) b.x)) x))
         "SELECT (SELECT 1) AS a, (SELECT b.x FROM b AS b) AS x"))

      (test-case "emits subqueries in from clauses"
        (check-emitted
         (~> (from (subquery (select _ (as 1 x))) #:as a)
             (select a.x))

         "SELECT a.x FROM (SELECT 1 AS x) AS a"))

      (test-case "allows queries to be composed"
        (define active-usernames
          (~> (from "users" #:as u)
              (select u.username)
              (group-by u.username)))

        (check-emitted
         (~> (from (subquery active-usernames) #:as a)
             (select (count a.*)))

         "SELECT COUNT(a.*) FROM (SELECT u.username FROM users AS u GROUP BY u.username) AS a"))))

    (test-suite
     "join"

     (test-case "emits inner join clauses"
       (check-emitted
        (~> (from "books" #:as b)
            (join "authors" #:as a #:on (= b.author-id a.id))
            (select a.name (count b.title))
            (group-by a.id))

        "SELECT a.name, COUNT(b.title) FROM books AS b JOIN authors AS a ON b.author_id = a.id GROUP BY a.id"))

     (test-case "emits multiple join clauses"
       (check-emitted
        (~> (from "books" #:as b)
            (join "authors" #:as a #:on (= b.author-id a.id))
            (join "author_pics" #:as ap #:on (= a.id ap.author-id))
            (select a.name ap.picture (count b.title))
            (group-by a.id ap.picture))

        "SELECT a.name, ap.picture, COUNT(b.title) FROM books AS b JOIN authors AS a ON b.author_id = a.id JOIN author_pics AS ap ON a.id = ap.author_id GROUP BY a.id, ap.picture"))

     (test-case "emits different types of joins"
       (check-emitted
        (~> (from "posts" #:as p)
            (join #:left "comments" #:as c #:on (= p.id c.post-id))
            (select a.* c.*))

        "SELECT a.*, c.* FROM posts AS p LEFT JOIN comments AS c ON p.id = c.post_id"))

     (test-case "emits joins for subqueries"
       (check-emitted
        (~> (from "posts" #:as p)
            (join #:left (subquery (select _ 1)) #:as c #:on #t)
            (select p.* c.*))

        "SELECT p.*, c.* FROM posts AS p LEFT JOIN (SELECT 1) AS c ON TRUE"))

     (test-case "emits lateral joins"
       (check-emitted
        (~> (from "posts" #:as p)
            (join #:left
                  #:lateral
                  (subquery (~> (from "post_images" #:as pi)
                                (where (= pi.post-id p.id))
                                (order-by ([pi.id #:desc]))
                                (limit 1)))
                  #:as pi
                  #:on #t)
            (select p.* pi.*))
        "SELECT p.*, pi.* FROM posts AS p LEFT JOIN LATERAL (SELECT * FROM post_images AS pi WHERE pi.post_id = p.id ORDER BY pi.id DESC LIMIT 1) AS pi ON TRUE")))

    (test-suite
     "group-by"

     (test-case "adds a group-by clause to the given query"
       (check-emitted
        (~> (from "books" #:as b)
            (select b.year (count b.title))
            (group-by b.year))
        "SELECT b.\"year\", COUNT(b.title) FROM books AS b GROUP BY b.\"year\""))

     (test-case "augments existing group-by clauses"
       (check-emitted
        (~> (from "books" #:as b)
            (select b.year b.month (count *))
            (group-by b.year)
            (group-by b.month))
        "SELECT b.\"year\", b.\"month\", COUNT(*) FROM books AS b GROUP BY b.\"year\", b.\"month\"")))

    (test-suite
     "order-by"

     (test-case "adds an order-by clause to the given query"
       (check-emitted
        (~> (from "books" #:as b)
            (select b.title)
            (order-by ([b.year])))
        "SELECT b.title FROM books AS b ORDER BY b.\"year\""))

     (test-case "augments existing order-by clauses"
       (check-emitted
        (~> (from "books" #:as b)
            (select b.title)
            (order-by ([b.year #:desc]))
            (order-by ([b.title])))
        "SELECT b.title FROM books AS b ORDER BY b.\"year\" DESC, b.title"))

     (test-case "supports descending directions"
       (check-emitted
        (~> (from "books" #:as b)
            (select b.title)
            (order-by ([b.year #:desc]
                       [b.title])))
        "SELECT b.title FROM books AS b ORDER BY b.\"year\" DESC, b.title"))

     (test-case "supports explicit #:asc directions"
       (check-emitted
        (~> (from "books" #:as b)
            (select b.title)
            (order-by ([b.year #:desc]
                       [b.title #:asc])))
        "SELECT b.title FROM books AS b ORDER BY b.\"year\" DESC, b.title"))

     (test-case "supports dynamic directions"
       (define order 'desc)

       (check-emitted
        (~> (from "books" #:as b)
            (order-by ([b.year ,order])))
        "SELECT * FROM books AS b ORDER BY b.\"year\" DESC"))

     (test-case "errors out when given an invalid dynamic direction"
       (check-exn
        exn:fail:contract?
        (lambda ()
          (~> (from "books" #:as b)
              (order-by ([b.year ,1]))))))

     (test-case "supports dynamic columns"
       (define column (ast:qualified "b" "title"))

       (check-emitted
        (~> (from "books" #:as b)
            (order-by ([(fragment column) #:desc])))
        "SELECT * FROM books AS b ORDER BY b.title DESC"))

     (test-case "errors out when given an invalid dynamic column"
       (check-exn
        exn:fail:contract?
        (lambda ()
          (~> (from "books" #:as b)
              (order-by ([(fragment 1)]))))))

     (test-case "supports NULLS FIRST"
       (define q (from "books" #:as b))
       (define result
         "SELECT * FROM books AS b ORDER BY b.title NULLS FIRST, b.\"year\"")
       (check-emitted (order-by q ([b.title #:asc #:nulls-first] [b.year])) result)
       (check-emitted (order-by q ([b.title #:asc ,'nulls-first] [b.year])) result)
       (check-emitted (order-by q ([b.title ,'asc ,'nulls-first] [b.year])) result)
       (check-emitted (order-by q ([b.title ,'asc #:nulls-first] [b.year])) result))

     (test-case "supports NULLS LAST"  ; ASC NULLS LAST is actually Postgres' default
       (define q (from "books" #:as b))
       (define result
         "SELECT * FROM books AS b ORDER BY b.title NULLS LAST, b.\"year\"")
       (check-emitted (order-by q ([b.title #:asc #:nulls-last] [b.year])) result))

     (test-case "supports DESC NULLS LAST"
       (define q (from "books" #:as b))
       (define result
         "SELECT * FROM books AS b ORDER BY b.title DESC NULLS LAST, b.\"year\"")
       (check-emitted (order-by q ([b.title #:desc #:nulls-last] [b.year])) result)
       (check-emitted (order-by q ([b.title ,'desc ,'nulls-last] [b.year])) result)
       (check-emitted (order-by q ([b.title #:desc ,'nulls-last] [b.year])) result)
       (check-emitted (order-by q ([b.title ,'desc #:nulls-last] [b.year])) result))

     (test-case "errors out when given an invalid dynamic nulls direction"
       (check-exn
        exn:fail:contract?
        (lambda ()
          (~> (from "books" #:as b)
              (order-by ([b.title #:asc ,'foo]))))))

     (test-case "supports dynamic lists"
       (check-emitted
        (~> (from "books" #:as b)
            (where (in b.id ,@(list 1 2 3))))
        "SELECT * FROM books AS b WHERE b.id IN (1, 2, 3)")

       (check-emitted
        (~> (from "books" #:as b)
            (where (in b.title ,@(list "a" "b" "c"))))
        "SELECT * FROM books AS b WHERE b.title IN ('a', 'b', 'c')")))

    (test-suite
     "offset"

     (check-emitted
      (~> (from "books" #:as b)
          (select b.title)
          (offset 20)
          (order-by ([b.title])))

      "SELECT b.title FROM books AS b ORDER BY b.title OFFSET 20"))

    (test-suite
     "limit"

     (check-emitted
      (~> (from "books" #:as b)
          (limit 20))

      "SELECT * FROM books AS b LIMIT 20")

     (check-emitted
      (~> (from "books" #:as b)
          (offset 10)
          (limit 20))

      "SELECT * FROM books AS b LIMIT 20 OFFSET 10")

     (check-emitted/placeholders
      (~> (from "books" #:as b)
          (limit ,(add1 10)))

      "SELECT * FROM books AS b LIMIT $1" '(11))

     (check-emitted/placeholders
      (~> (from "books" #:as b)
          (offset ,1)
          (limit ,(add1 10)))

      "SELECT * FROM books AS b LIMIT $1 OFFSET $2" '(11 1)))

    (test-suite
     "placeholders"

     (check-emitted/placeholders
      (select _ ,42)
      "SELECT $1"
      '(42))

     (let ([x 1]
           [y "hello"])
       (check-emitted/placeholders
        (select _ (<> ,x ,y))
        "SELECT $1 <> $2"
        '(1 "hello"))))

    (test-suite
     "union"

     (check-emitted
      (union (select _ 1)
             (select _ 2))
      "SELECT 1 UNION (SELECT 2)")

     (check-emitted
      (union
       (union (select _ 1)
              (select _ 2))
       (select _ 3))
      "SELECT 1 UNION (SELECT 2 UNION (SELECT 3))")

     (check-emitted
      (union
       (union
        (union (select _ 1)
               (select _ 2))
        (select _ 3))
       (select _ 4))
      "SELECT 1 UNION (SELECT 2 UNION (SELECT 3 UNION (SELECT 4)))")

     (check-emitted
      (~> (select _ 1)
          (union (select _ 2))
          (union (select _ 3))
          (union (select _ 4)))
      "SELECT 1 UNION (SELECT 2 UNION (SELECT 3 UNION (SELECT 4)))"))

    (test-case "quotes reserved keywords"
      (check-emitted
       (~> (from "reserved" #:as r)
           (select r.user r.timestamp))
       "SELECT r.\"user\", r.\"timestamp\" FROM reserved AS r"))

    (test-case "does not quote reserved keywords unless qualified"
      (check-emitted
       (select _ current_timestamp)
       "SELECT CURRENT_TIMESTAMP")
      (check-emitted
       (select _ (= user "postgresql"))
       "SELECT USER = 'postgresql'")))

   (test-suite
    "select-for-schema"

    (let ()
      (define-schema example
        #:virtual
        ([a integer/f]
         [b string/f]))

      (check-emitted
       (~> (from "examples" #:as e)
           (select-for-schema ,example-schema #:from e))
       "SELECT e.a, e.b FROM examples AS e")

      (check-emitted
       (~> (from "examples" #:as e)
           (select-for-schema ,example-schema
                              #:from e
                              #:customizing
                              ([b 42])))
       "SELECT e.a, 42 FROM examples AS e")))

   (test-suite
    "update"

    (check-emitted
     (~> (from "users" #:as u)
         (update [username ,1]
                 [password-hash ,2]))

     "UPDATE users AS u SET username = $1, password_hash = $2")

    (check-emitted
     (~> (from "users" #:as u)
         (update [username ,1]
                 [password-hash ,2])
         (where (= u.id ,3)))

     "UPDATE users AS u SET username = $1, password_hash = $2 WHERE u.id = $3")

    (check-emitted
     (~> (from "users" #:as u)
         (update [username ,1]
                 [password-hash ,2])
         (where (= u.id ,3))
         (or-where (= u.id ,4)))

     "UPDATE users AS u SET username = $1, password_hash = $2 WHERE (u.id = $3) OR (u.id = $4)")

    (check-emitted
     (~> (from "users" #:as u)
         (update [username "bill"])
         (where (= u.id 1))
         (returning u.username))

     "UPDATE users AS u SET username = 'bill' WHERE u.id = 1 RETURNING u.username")

    (test-case "raises an error if trying to update a subquery"
      (check-exn
       exn:fail:contract?
       (lambda ()
         (~> (from (subquery (select _ 1)) #:as t)
             (update [x 2]))))))

   (test-suite
    "delete"

    (test-case "generates DELETE queries"
      (check-emitted
       (delete (from "users" #:as u))

       "DELETE FROM users AS u"))

    (test-case "supports WHERE clauses"
      (check-emitted
       (~> (delete (from "users" #:as u))
           (where (not u.active?)))

       "DELETE FROM users AS u WHERE NOT u.is_active"))

    (test-case "supports RETURNING clauses"
      (check-emitted
       (~> (delete (from "users" #:as u))
           (where u.active?)
           (returning u.id))

       "DELETE FROM users AS u WHERE u.is_active RETURNING u.id"))

    (test-case "supports the augmentation of RETURNING clauses"
      (check-emitted
       (~> (delete (from "users" #:as u))
           (where u.active?)
           (returning u.id)
           (returning u.username u.last-logged-in))

       "DELETE FROM users AS u WHERE u.is_active RETURNING u.id, u.username, u.last_logged_in"))

    (test-case "raises an error if trying to delete a subquery"
      (check-exn
       exn:fail:contract?
       (lambda ()
         (delete (from (subquery (select _ 1)) #:as t))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests sql-tests))
