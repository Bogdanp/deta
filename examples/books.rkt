#lang racket/base

(require db
         deta
         threading)

(define-schema book
  ([id id/f #:primary-key #:auto-increment]
   [title string/f]
   [author string/f]
   [year-published integer/f #:nullable]))

(define conn
  (sqlite3-connect #:database 'memory))

(void
 (create-table! conn 'book)
 (insert! conn
          (make-book #:title "To Kill a Mockingbird"
                     #:author "Harper Lee"
                     #:year-published 1960)
          (make-book #:title "1984"
                     #:author "George Orwell"
                     #:year-published 1949)
          (make-book #:title "The Lord of the Rings"
                     #:author "J.R.R. Tolkien"
                     #:year-published 1955)
          (make-book #:title "The Catcher in the Rye"
                     #:author "J.D. Salinger"
                     #:year-published 1949)))

(define (books-before year)
  (displayln (format "Books published before ~a:" year))
  (for ([b (in-rows conn (~> (from book #:as b)
                             (where (< b.year-published ,year))))])
    (displayln (book-title b))))

(define (books-between start-year end-year)
  (displayln (format "Books published between ~a and ~a:" start-year end-year))
  (for ([b (in-rows conn (~> (from book #:as b)
                             (where (between b.year-published 1950 1970))))])
    (displayln (book-title b))))

(books-before 1950)
(displayln "")
(books-between 1950 1970)
