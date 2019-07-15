#lang racket/base

(require db
         deta
         racket/sequence
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
  (~> (from book #:as b)
      (where (< b.year-published ,year))))

(define (books-between start-year end-year)
  (~> (from book #:as b)
      (where (between b.year-published ,start-year ,end-year))))

(displayln "Books published before 1950:")
(for ([b (in-entities conn (books-before 1950))])
  (displayln (book-title b)))

(displayln "")
(displayln "Books published between 1950 and 1970:")
(for ([b (in-entities conn (books-between 1950 1970))])
  (displayln (book-title b)))

(define-schema book-stats
  #:virtual
  ([year integer/f]
   [books integer/f]))

(displayln "")
(displayln "Statistics:")
(for ([stats (in-entities conn (~> (from book #:as b)
                                   (select b.year-published (count b.title))
                                   (group-by b.year-published)
                                   (order-by ([b.year-published #:desc]))
                                   (project-onto book-stats-schema)))])
  (displayln (format "year: ~a books: ~a"
                     (book-stats-year stats)
                     (book-stats-books stats))))

(query-exec conn (delete (books-between 1950 1970)))

(displayln "")
(displayln "Books published between 1950 and 1970:")
(for ([b (in-entities conn (books-between 1950 1970))])
  (displayln (book-title b)))
