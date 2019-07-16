#lang scribble/manual

@(require scribble/example
          racket/format
          racket/runtime-path
          racket/sandbox
          (for-label db
                     deta
                     gregor
                     json
                     (except-in racket/base date date?)
                     racket/contract
                     racket/match
                     racket/sequence
                     threading))

@title{deta -- Functional Database Mapping}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

@defmodule[deta]

@(define (repo-link label)
   (hyperlink "https://github.com/Bogdanp/deta" label))

This library automatically maps database tables to Racket structs and
lets you perform CRUD operations on them as well as arbitrary queries.
Sort of like an ORM, but without "relationships" and all the bad bits.

The API is currently fairly stable, but it may change before 1.0.
Watch the @repo-link{GitHub repository} if you want to stay on top of
potential changes.


@section[#:tag "principles"]{Principles}

Despite the fact that both SQLite and PostgreSQL are currently
supported by this library, the focus is on PostgreSQL and SQLite just
serves as a check to ensure that nothing is @emph{too} PostgreSQL
specific.

Additionally, the purpose of deta is to make certain types of
normally-tedious queries straightforward and simple.  It's not a goal
of this library to be a general purpose SQL DSL and that means that
for some queries you may have to resort to raw SQL or the
@racketmodname[sql] library.  It is very much an "80% solution."

Being externally-extensible is also not a goal.  The SQL AST as well
as all of the dialect code is considered private and any new dialects
(such as MySQL) will have to be added to the library itself.

If you're down with that, then by all means carry on an read the
tutorial!


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
(for/list ([b (in-entities conn (~> (from book #:as b)
                                    (where (< b.published-on (date "1955-01-01")))
                                    (order-by ([b.published-on #:desc]))))])
  (book-title b))
]

Sweet!  Here's the query we just ran:

@interaction[
#:eval db-eval
(displayln
 (~> (from book #:as b)
     (where (< b.published-on (date "1955-01-01")))
     (order-by ([b.published-on #:desc]))))
]

What about dynamic parameters, you may ask?  Let's turn the above into
a function:

@interaction[
#:eval db-eval
(define (books-before year)
  (~> (from book #:as b)
      (where (< b.published-on ,(sql-date year 1 1)))
      (order-by ([b.published-on #:desc]))))

(code:line)
(for/list ([b (in-entities conn (books-before 1950))])
  (book-title b))

(code:line)
(for/list ([b (in-entities conn (books-before 1955))])
  (book-title b))
]

Any time the query combinators encounter an @racket[unquote], that
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
(define books-published-by-year
  (~> (from book #:as b)
      (select (as
                (cast (date_trunc "year" b.published-on) date)
                year)
              (count b.title))
      (group-by year)
      (order-by ([year]))
      (project-onto book-stats-schema)))

(code:line)
(for ([s (in-entities conn books-published-by-year)])
  (displayln (format "year: ~a books: ~a"
                     (book-stats-year s)
                     (book-stats-books s))))
]

Finally, let's delete all the books published before 1950:

@interaction[
#:eval db-eval
(query-exec conn (delete (books-before 1950)))
]

And re-run the last query:

@interaction[
#:eval db-eval
(for ([s (in-entities conn books-published-by-year)])
  (displayln (format "year: ~a books: ~a"
                     (book-stats-year s)
                     (book-stats-books s))))
]

That's it! You now know the basics of deta.  Thanks for following
along!  If you want to learn more, check out the reference
documentation below.


@section[#:tag "versus"]{Compared to *}

@subsection{sql}

@racketmodname[sql] is great at statically generating SQL queries.
The problem is that the generated queries are not composable at
runtime.  You have to write macros upon macros to handle composition
and I've found that that gets tedious quickly.

On top of giving you composable queries -- as you can hopefully see
from the tutorial --, deta also automatically maps CRUD operations to
structs, which is out of scope for @racketmodname[sql].


@section[#:tag "todos"]{Notes and TODOs}

@(define (insert-link label)
   (hyperlink "https://www.postgresql.org/docs/11/sql-insert.html" label))

@(define (select-link label)
   (hyperlink "https://www.postgresql.org/docs/11/sql-select.html" label))

@(define (update-link label)
   (hyperlink "https://www.postgresql.org/docs/11/sql-update.html" label))

@(define (delete-link label)
   (hyperlink "https://www.postgresql.org/docs/11/sql-delete.html" label))

Subqueries are not currently supported, neither are @tt{VALUES}
expressions.

The following @select-link{@tt{SELECT}} clauses are not currently
supported:

@itemlist[
  @item{@tt{WITH}}
  @item{@tt{JOIN}}
  @item{@tt{HAVING}}
  @item{@tt{WINDOW}}
  @item{@tt{UNION}}
  @item{@tt{INTERSECT}}
  @item{@tt{EXCEPT}}
  @item{@tt{FOR UPDATE}}
]

The following @update-link{@tt{UPDATE}} clauses are not currently
supported in arbitrary queries:

@itemlist[
  @item{@tt{WITH}}
  @item{@tt{FROM}}
]

The following @delete-link{@tt{DELETE}} clauses are not currently
supported in arbitrary queries:

@itemlist[
  @item{@tt{WITH}}
]

Arbitrary @insert-link{@tt{INSERT}} statements are not currently
supported.

Arbitrary column constraints and @emph{before-{delete,persist}} style
hooks will be supported at some point.


@section[#:tag "reference"]{Reference}

@subsection{Query}
@defmodule[deta/query]

@subsubsection{DDL}

@defproc[(create-table! [conn connection?]
                        [schema (or/c schema? symbol?)]) void?]{

  Creates the table represented by @racket[schema] if it does not
  exist.  If @racket[schema] is a symbol, then it is looked up in the
  global registry.
}

@defproc[(drop-table! [conn connection?]
                      [schema (or/c schema? symbol?)]) void?]{

  Drops the table represented by @racket[schema] if it exists.
}

@subsubsection{Entity CRUD}

@defproc[(insert! [conn connection?]
                  [e entity?] ...) (listof entity?)]{

  Attempts to insert any newly-created entities into the database,
  returning the ones that were persisted.  Entities that have already
  been persisted are ignored.

  Raises a user error if any of the entities are based on virtual
  schemas.
}

@defproc[(insert-one! [conn connection?]
                      [e entity?]) (or/c false/c entity?)]{

  Attempts to insert @racket[e].  If it doesn't need to be persisted,
  then @racket[#f] is returned.

  Equivalent to:

  @racketblock[
    (match (insert! conn e)
      [(list e) e]
      [_ #f])
  ]
}

@defproc[(in-entities [conn connection?]
                      [q query?]
                      [#:batch-size batch-size (or/c exact-positive-integer? +inf.0) +inf.0]) sequence?]{

  Queries the database and, based on @racket[q], either returns a
  sequence of entities or a sequence of @racket[values].

  @racket[#:batch-size] controls how many rows to fetch from the
  databaase at a time.  It is analogous to @racket[in-query]'s
  @racket[#:fetch] argument.
}

@defproc[(lookup [conn connection?]
                 [q query?]) (or/c false/c entity?)]{

  Retrieves the first result for @racket[q], if any.
}

@defproc[(update! [conn connection?]
                  [e entity?] ...) (listof entity?)]{

  Attempts to update any modified entities.  Only updates the fields
  that have changed since the entities were retrieved from the
  database.  Returns those entities that have been updated.

  Raises a user error if any of the entities don't have a primary key
  field.
}

@defproc[(update-one! [conn connection?]
                      [e entity?]) (or/c false/c entity?)]{

  Attempts to update @racket[e].  If it doesn't need to be updated,
  then @racket[#f] is returned.

  Equivalent to:

  @racketblock[
    (match (update! conn e)
      [(list e) e]
      [_ #f])
  ]
}

@defproc[(delete! [conn connection?]
                  [e entity?] ...) (listof entity?)]{

  Attempts to delete any previously-persisted entities.  Returns those
  entities that have been deleted.

  Raises a user error if any of the entities don't have a primary key
  field.
}

@defproc[(delete-one! [conn connection?]
                      [e entity?]) (or/c false/c entity?)]{

  Attempts to delete @racket[e].  If it doesn't need to be deleted,
  then @racket[#f] is returned.

  Equivalent to:

  @racketblock[
    (match (delete! conn e)
      [(list e) e]
      [_ #f])
  ]
}


@subsubsection{Query Combinators}

@defproc[(query? [q any/c]) boolean?]{
  Returns @racket[#t] when @racket[q] is a query.
}

@defform[
  #:literals (array as and case else list or unquote)
  (sql q-expr)
  #:grammar
  [(q-expr (array q-expr ...)
           (as q-expr id)
           (and q-expr q-expr)
           (case [q-expr q-expr] ...+)
           (case [q-expr q-expr] ...+
                 [else q-expr])
           (or q-expr q-expr)
           (list q-expr ...)
           (unquote expr)
           id
           boolean
           string
           number
           app)
   (app (q-expr q-expr ...))]]{

  Constructs an SQL expression.
}

@defproc[(delete [q query?]) query?]{

  Converts @racket[q] into a @tt{DELETE} query, preserving its
  @tt{FROM} and @tt{WHERE} clauses.

  An error is raised if @racket[q] is anything other than a
  @tt{SELECT} query.
}

@defform[
  (from schema #:as alias)
  #:grammar
  [(schema (code:line string)
           (code:line id))]]{

  Creates a new @tt{SELECT} @racket[query?] from a schema or a table name.
}

@defform[
  (update query assignment ...+)
  #:grammar
  [(assignment [column q-expr])]]{

  Converts @racket[query] into an @tt{UPDATE} query, preserving its
  @tt{FROM} clause, making it the target table for the update, and its
  @tt{WHERE} clause.

  An error is raised if @racket[q] is anything other than a
  @tt{SELECT} query.
}

@defform*[
  #:literals (_)
  ((select _ q-expr ...+)
   (select query q-expr ...+))
]{
  Refines the set of selected values in @racket[query].

  The first form (with the @racket[_]) generates a fresh query.

  @examples[
    (require deta)
    (displayln (select _ 1 2))
    (displayln (select (from "users" #:as u) u.username))
  ]
}

@defform[(group-by query q-expr ...+)]{
  Adds or replaces a @tt{GROUP BY} clause to @racket[query].
}

@defform[
  (order-by query ([column maybe-direction] ...+))
  #:grammar
  [(maybe-direction (code:line)
                    (code:line #:asc)
                    (code:line #:desc))]]{
  Adds or replaces an @tt{ORDER BY} clause to @racket[query].
}

@defform[(limit query n)]{
  Adds or replaces a @tt{LIMIT @racket[n]} clause to @racket[query].
}

@defform[(offset query n)]{
  Adds or replaces an @tt{OFFSET @racket[n]} clause to @racket[query].
}

@defform[(where query q-expr)]{
  Adds or replaces a @tt{WHERE} clause to @racket[query].
}

@defform[(and-where query q-expr)]{
  Wraps the @tt{WHERE} clause in @racket[query] to the result of
  @tt{AND}-ing it with @racket[q-expr].
}

@defform[(or-where query q-expr)]{
  Wraps the @tt{WHERE} clause in @racket[query] to the result of
  @tt{OR}-ing it with @racket[q-expr].
}

@defproc[(project-onto [q query?]
                       [s schema?]) query?]{
  Changes the target schema for @racket[q] to @racket[s].
}

@defform[(returning query q-expr ...+)]{
  Adds or replaces a @tt{RETURNING} clause to @racket[query].
}

@subsection{Schema}
@defmodule[deta/schema]

@defproc[(entity? [e any/c]) boolean?]{
  Returns @racket[#t] when @racket[e] is an instance of a schema
  struct (i.e. an "entity").
}

@defproc[(schema? [s any/c]) boolean?]{
  Returns @racket[#t] when @racket[s] is a schema.
}

@defform[(define-schema id
           maybe-table
           maybe-virtual
           (field-definition ...+))
         #:grammar
         [(maybe-table (code:line)
                       (code:line #:table string))
          (maybe-virtual (code:line)
                         (code:line #:virtual))
          (field-definition (code:line [id type
                                        maybe-name
                                        maybe-primary-key
                                        maybe-auto-increment
                                        maybe-unique
                                        maybe-nullable
                                        maybe-contract
                                        maybe-wrapper])
                            (code:line [(id default) type
                                        maybe-name
                                        maybe-primary-key
                                        maybe-auto-increment
                                        maybe-unique
                                        maybe-nullable
                                        maybe-contract
                                        maybe-wrapper]))
          (maybe-name (code:line)
                      (code:line #:name string))
          (maybe-primary-key (code:line)
                             (code:line #:primary-key))
          (maybe-auto-increment (code:line)
                                (code:line #:auto-increment))
          (maybe-unique (code:line)
                        (code:line #:unique))
          (maybe-nullable (code:line)
                          (code:line #:nullable))
          (maybe-contract (code:line)
                          (code:line #:contract e))
          (maybe-wrapper (code:line)
                          (code:line #:wrapper e))]]{

  Defines a schema named @racket[id].  The schema will have an
  associated struct with the same name and a smart constructor called
  @tt{make-@emph{id}}.  The struct's "dumb" constructor is hidden so
  that invalid entities cannot be created.

  For every defined field there will be an associated functional
  setter and updater named @tt{set-@emph{id}-@emph{field}} and
  @tt{update-@emph{id}-@emph{field}}, respectively.

  If @racket[#:table] is provided, then that is used as the name for
  the table.  Otherwise, an "s" is appended to the schema id to
  pluralize it.  Currently, there are no other pluralization rules.

  If @racket[#:virtual] is provided, then the resulting schema's
  entities will not be able to be persisted, nor will the schema be
  registered in the global registry.

  A syntax error is raised if you declare a field as both a primary
  key and nullable.  Additionally, a syntax error is raised if a
  schema has multiple primary keys.

  Every type has an associated contract so the @racket[#:contract]
  option for fields is only necessary if you want to further restrict
  the values that a field can contain.

  When converting field names to SQL, dashes are replaced with
  underscores and field names that end in question marks drop their
  question mark and are prefixed with "is_".  @racket[admin?] becomes
  @racket[is_admin].

  Custom field names can be specified by providing a @racket[#:name]
  in the field definition.  Note, however, that the library does not
  currently translate between field names and custom column names
  within arbitrary queries.

  Example:

  @racketblock[
    (define-schema book
      ([id id/f #:primary-key #:auto-increment]
       [title string/f #:unique #:contract non-empty-string? #:wrapper string-titlecase]
       [author string/f #:contract non-empty-string?]))
  ]
}

@defform[(schema-out schema)]{
  Exports all bindings related to @racket[schema].

  @interaction[
  (module sub racket/base
    (require deta)
    (provide (schema-out album))

    (define-schema album
      #:virtual
      ([id id/f #:primary-key #:auto-increment]
       [title string/f]
       [band string/f])))

  (code:line)
  (require 'sub)
  (define an-album
    (make-album #:title "Led Zeppelin"
                #:band "Led Zeppelin"))

  (code:line)
  (album? an-album)
  (album-title an-album)
  (album-title (update-album-title an-album string-upcase))]
}


@subsection{Type}
@defmodule[deta/type]

These are all the field types currently supported by deta.  Note that
not all database backends support all of these types.

@subsubsection{Support Matrix}

Here are all the types and how they map to the different backends.

@tabular[
  #:sep @hspace[2]
  (list (list @bold{Field Type}       @bold{Racket Type}                   @bold{PostgreSQL Type}  @bold{SQLite Type})
        (list @racket[id/f]           @racket[exact-nonnegative-integer?]  @tt{INTEGER / SERIAL}   @tt{INTEGER}      )
        (list @racket[integer/f]      @racket[exact-integer?]              @tt{INTEGER}            @tt{INTEGER}      )
        (list @racket[real/f]         @racket[real?]                       @tt{REAL}               @tt{REAL}         )
        (list @racket[numeric/f]      @racket[(or/c rational? +nan.0)]     @tt{NUMERIC}            @tt{UNSUPPORTED}  )
        (list @racket[string/f]       @racket[string?]                     @tt{TEXT}               @tt{TEXT}         )
        (list @racket[binary/f]       @racket[bytes?]                      @tt{BLOB}               @tt{BLOB}         )
        (list @racket[symbol/f]       @racket[symbol?]                     @tt{TEXT}               @tt{TEXT}         )
        (list @racket[boolean/f]      @racket[boolean?]                    @tt{BOOLEAN}            @tt{INTEGER}      )
        (list @racket[date/f]         @racket[date-provider?]              @tt{DATE}               @tt{TEXT}         )
        (list @racket[time/f]         @racket[time-provider?]              @tt{TIME}               @tt{TEXT}         )
        (list @racket[datetime/f]     @racket[datetime-provider?]          @tt{TIMESTAMP}          @tt{TEXT}         )
        (list @racket[datetime-tz/f]  @racket[moment-provider?]            @tt{TIMESTMAPTZ}        @tt{TEXT}         )
        (list @racket[array/f]        @racket[vector?]                     @tt{ARRAY}              @tt{UNSUPPORTED}  )
        (list @racket[json/f]         @racket[jsexpr?]                     @tt{JSON}               @tt{UNSUPPORTED}  )
        (list @racket[jsonb/f]        @racket[jsexpr?]                     @tt{JSONB}              @tt{UNSUPPORTED}  )
        )]

@subsubsection{Types}

@deftogether[
  (@defproc[(type? [v any/c]) boolean?]
   @defthing[id/f type?]
   @defthing[integer/f type?]
   @defthing[real/f type?]
   @defproc[(numeric/f [precision exact-integer?]
                       [scale exact-integer?]) type?]
   @defthing[string/f type?]
   @defthing[binary/f type?]
   @defthing[symbol/f type?]
   @defthing[boolean/f type?]
   @defthing[date/f type?]
   @defthing[time/f type?]
   @defthing[datetime/f type?]
   @defthing[datetime-tz/f type?]
   @defproc[(array/f [t type?]) type?]
   @defthing[json/f type?]
   @defthing[jsonb/f type?])]{

  The various types that deta supports.
}
