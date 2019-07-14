#lang scribble/manual

@(require scribble/example
          racket/runtime-path
          racket/sandbox
          (for-label db
                     deta
                     gregor
                     (except-in racket/base date date?)
                     racket/match
                     threading))

@title{deta -- Functional Database Mapping}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

@defmodule[deta]

This library automatically maps database tables to Racket structs and
lets you perform CRUD operations on them as well as arbitrary queries.
Sort of like an ORM, but without "relationships" and all the bad bits.

This is pre-1.0 software and the API may change without notice.

@section[#:tag "tutorial"]{Tutorial}

@; Blatantly copied from sql-lib!
@(begin
   (define-syntax-rule (interaction e ...) (examples #:label #f e ...))
   (define-runtime-path log-file "tutorial-log.rktd")
   (define log-mode 'replay)
   (define (make-pg-eval log-file)
     (let ([ev (make-log-based-eval log-file log-mode)])
       (ev '(require db deta racket/contract racket/match racket/string threading))
       ev))
   (define db-eval (make-pg-eval log-file)))

deta builds upon the @racketmodname[db] library.  You will use deta to
generate your mappings and create queries, but the database library
will be doing the actual work of talking to the database and handling
transactions.

Let's start by creating a database connection in the usual way.

@interaction[
#:eval db-eval
(require db)

(code:line)
(define conn
  (postgresql-connect #:database "deta"
                      #:user     "deta"
                      #:password "deta"))
]

Next, let's define a schema for books:

@interaction[
#:eval db-eval
(require deta)

(code:line)
(define-schema book
  ([id id/f #:primary-key #:auto-increment]
   [title string/f #:contract non-empty-string? #:wrapper string-titlecase]
   [author string/f #:contract non-empty-string?]
   [published-on date/f]))
]

The above will generate a struct named @racket[book] with fields for
@racket[id], @racket[title], @racket[author] and @racket[published-on],
an associated "smart constructor" called @racket[make-book] and
functional setter and updater functions for each field.

@interaction[
#:eval db-eval
(require gregor)

(code:line)
(define a-book
  (make-book #:title "To Kill a Mockingbird"
             #:author "Harper Lee"
             #:published-on (date 1960 7 11)))

(code:line)
(book-id a-book)
(book-title a-book)
(book-title (update-book-title a-book (lambda (t)
                                        (string-append t "?"))))

(code:line)
(code:comment "schema entities are immutable so the above did not change a-book")
(book-title a-book)
]

We can use the schema to issue DDL commands to the database and create
the table:

@interaction[
#:eval db-eval
(code:comment "drop it in case it already exists")
(drop-table! conn 'book)

(code:line)
(code:comment "create the table")
(create-table! conn 'book)
]

And now that we have a table, we can insert the book that we created
into the database:

@interaction[
#:eval db-eval
(match-define (list saved-book)
  (insert! conn a-book))

(code:line)
(book-id saved-book)
]

Let's insert a few more books:

@interaction[
#:eval db-eval
(void
 (insert! conn (make-book #:title "1984"
                          #:author "George Orwell"
                          #:published-on (date 1949 6 8))
               (make-book #:title "The Lord of the Rings"
                          #:author "J.R.R. Tolkien"
                          #:published-on (date 1954 7 29))
               (make-book #:title "The Catcher in the Rye"
                          #:author "J.D. Salinger"
                          #:published-on (date 1949 7 16))))
]

And now let's query for all of the books published before 1955:

@interaction[
#:eval db-eval
(require threading)

(code:line)
(for/list ([b (in-rows conn (~> (from book #:as b)
                                (where (< b.published-on (cast "1955-01-01" date)))
                                (order-by ([b.published-on #:desc]))))])
  (book-title b))
]

Sweet!  Here's the query we just ran:

@interaction[
#:eval db-eval
(displayln
 (~> (from book #:as b)
     (where (< b.published-on (cast "1955-01-01" date)))
     (order-by ([b.published-on #:desc]))))
]

What about dynamic parameters, you may ask?  Let's turn the above into
a function:

@interaction[
#:eval db-eval
(define (books-before year)
  (in-rows conn (~> (from book #:as b)
                    (where (< b.published-on ,(sql-date year 1 1)))
                    (order-by ([b.published-on #:desc])))))

(code:line)
(for/list ([b (books-before 1950)])
  (book-title b))

(code:line)
(for/list ([b (books-before 1955)])
  (book-title b))
]

Any time the query combinators encounter an unquote (the comma), that
value gets replaced with a placeholder in the query and, when the
query is eventually executed, the value is bound to its prepared
statement.  Don't worry about it if that doesn't make too much sense
to you right now.  Just know that it's possible to use dynamic
parameters and that they are passed to the database securely.

Oftentimes, you'll want to query data from the DB that doesn't match
your schema.  Say we want to grab the number of books published by
year from our database.  To do that, we can declare a "virtual" schema
(one whose entities can't be persisted) and project our queries onto
that schema.

@interaction[
#:eval db-eval
(define-schema book-stats
  #:virtual
  ([year date/f]
   [books integer/f]))

(code:line)
(for ([s (in-rows conn (~> (from book #:as b)
                           (select (as
                                     (cast (date_trunc "year" b.published-on) date)
                                     year)
                                   (count b.title))
                           (group-by year)
                           (order-by ([year]))
                           (project-onto book-stats-schema)))])
  (displayln (format "year: ~a books: ~a"
                     (book-stats-year s)
                     (book-stats-books s))))
]

You now know the basics of deta.  Thanks for following along!  If you
want to learn more, check out the reference documentation below.


@section[#:tag "versus"]{Compared to *}

@subsection{sql}

@racketmodname[sql] is great at statically generating SQL queries.
The problem is that the generated queries are not composable at
runtime.  You have to write macros upon macros to handle composition
and I've found that that gets tedious quickly.

On top of giving you composable queries, deta also automatically maps
CRUD operations to structs, which is out of scope for
@racketmodname[sql].
