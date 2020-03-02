;; This file was created by make-log-based-eval
((require racket/contract
          racket/match
          racket/string
          threading
          (for-label db gregor))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((require db) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((define conn
   (postgresql-connect #:database "deta" #:user "deta" #:password "deta"))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((require deta) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((define-schema
  book
  ((id id/f #:primary-key #:auto-increment)
   (title string/f #:contract non-empty-string? #:wrapper string-titlecase)
   (author string/f #:contract non-empty-string?)
   (published-on date/f)))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((require gregor) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((define a-book
   (make-book
    #:title
    "To Kill a Mockingbird"
    #:author
    "Harper Lee"
    #:published-on
    (date 1960 7 11)))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((book-id a-book)
 ((3)
  1
  (((lib "db/private/generic/sql-data.rkt") . deserialize-info:sql-null-v0))
  0
  ()
  ()
  (c values c (0)))
 #""
 #"")
((book-title a-book)
 ((3) 0 () 0 () () (c values c (u . "To Kill A Mockingbird")))
 #""
 #"")
((book-title (update-book-title a-book (lambda (t) (string-append t "?"))))
 ((3) 0 () 0 () () (c values c (u . "To Kill A Mockingbird?")))
 #""
 #"")
((book-title a-book)
 ((3) 0 () 0 () () (c values c (u . "To Kill A Mockingbird")))
 #""
 #"")
((drop-table! conn 'book) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((create-table! conn 'book) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((define saved-book (insert-one! conn a-book))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((book-id saved-book) ((3) 0 () 0 () () (q values 1)) #"" #"")
((void
  (insert!
   conn
   (make-book
    #:title
    "1984"
    #:author
    "George Orwell"
    #:published-on
    (date 1949 6 8))
   (make-book
    #:title
    "The Lord of the Rings"
    #:author
    "J.R.R. Tolkien"
    #:published-on
    (date 1954 7 29))
   (make-book
    #:title
    "The Catcher in the Rye"
    #:author
    "J.D. Salinger"
    #:published-on
    (date 1949 7 16))))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((require threading) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((for/list
  ((b
    (in-entities
     conn
     (~>
      (from book #:as b)
      (where (< b.published-on (date "1955-01-01")))
      (order-by ((b.published-on #:desc)))))))
  (book-title b))
 ((3)
  0
  ()
  0
  ()
  ()
  (c
   values
   c
   (c
    (u . "The Lord Of The Rings")
    c
    (u . "The Catcher In The Rye")
    c
    (u . "1984"))))
 #""
 #"")
((displayln
  (~>
   (from book #:as b)
   (where (< b.published-on (date "1955-01-01")))
   (order-by ((b.published-on #:desc)))))
 ((3) 0 () 0 () () (c values c (void)))
 #"#<query: SELECT b.id, b.title, b.author, b.published_on FROM books AS b WHERE b.published_on < (DATE '1955-01-01') ORDER BY b.published_on DESC>\n"
 #"")
((define (books-before year)
   (~>
    (from book #:as b)
    (where (< b.published-on ,(sql-date year 1 1)))
    (order-by ((b.published-on #:desc)))))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((for/list ((b (in-entities conn (books-before 1950)))) (book-title b))
 ((3)
  0
  ()
  0
  ()
  ()
  (c values c (c (u . "The Catcher In The Rye") c (u . "1984"))))
 #""
 #"")
((for/list ((b (in-entities conn (books-before 1955)))) (book-title b))
 ((3)
  0
  ()
  0
  ()
  ()
  (c
   values
   c
   (c
    (u . "The Lord Of The Rings")
    c
    (u . "The Catcher In The Rye")
    c
    (u . "1984"))))
 #""
 #"")
((define-schema book-stats #:virtual ((year date/f) (books integer/f)))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define books-published-by-year
   (~>
    (from book #:as b)
    (select
     (as (cast (date_trunc "year" b.published-on) date) year)
     (count b.title))
    (group-by year)
    (order-by ((year)))
    (project-onto book-stats-schema)))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((for
  ((s (in-entities conn books-published-by-year)))
  (displayln
   (format "year: ~a books: ~a" (book-stats-year s) (book-stats-books s))))
 ((3) 0 () 0 () () (c values c (void)))
 #"year: #<date 1949-01-01> books: 2\nyear: #<date 1954-01-01> books: 1\nyear: #<date 1960-01-01> books: 1\n"
 #"")
((query-exec conn (delete (books-before 1950)))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((for
  ((s (in-entities conn books-published-by-year)))
  (displayln
   (format "year: ~a books: ~a" (book-stats-year s) (book-stats-books s))))
 ((3) 0 () 0 () () (c values c (void)))
 #"year: #<date 1954-01-01> books: 1\nyear: #<date 1960-01-01> books: 1\n"
 #"")
