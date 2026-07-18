#lang racket/base

(require db
         deta
         threading)

(define-schema artist
  ([id id/f #:primary-key #:auto-increment]
   [name string/f #:unique]))

(define-schema album
  ([id id/f #:primary-key #:auto-increment]
   [title string/f #:unique]
   [artist-id id/f #:foreign-key (artist id)]
   [year-published integer/f #:nullable]))

(define conn
  (sqlite3-connect #:database 'memory))

(void
 (create-all! conn)

 (insert! conn
          (make-album #:title "Nevermind"
                      #:artist-id (artist-id (insert-one! conn (make-artist #:name "Nirvana")))
                      #:year-published 1991)
          (make-album #:title "Achtung Baby"
                      #:artist-id (artist-id (insert-one! conn (make-artist #:name "U2")))
                      #:year-published 1991)
          (make-album #:title "The Miseducation of Lauryn Hill"
                      #:artist-id (artist-id (insert-one! conn (make-artist #:name "Lauryn Hill")))
                      #:year-published 1998)))

(define (albums-before year)
  (~> (from album #:as a)
      (where (< a.year-published ,year))))

(define (albums-between start-year end-year)
  (~> (from album #:as a)
      (where (between a.year-published ,start-year ,end-year))))

(displayln "Albums published before 1995:")
(for ([a (in-entities conn (albums-before 1995))])
  (displayln (album-title a)))

(displayln "")
(displayln "Albums published between 1990 and 2000:")
(for ([a (in-entities conn (albums-between 1990 2000))])
  (displayln (album-title a)))

(define-schema album-with-artists
  #:virtual
  ([id id/f #:primary-key #:auto-increment]
   [title string/f #:unique]
   [artist-id id/f #:foreign-key (artist id)]
   [year-published integer/f #:nullable]
   [name string/f]))

(displayln "")
(displayln "All albums with artist names:")
(for ([a (in-entities conn (~> (from album #:as al)
                               (join artist
                                     #:as at
                                     #:on (= al.artist_id at.id))
                               (select al.* at.name)
                               (project-onto album-with-artists-schema)))])
  (displayln (format "Artist: ~v, album: ~v"
                     (album-with-artists-name a)
                     (album-with-artists-title a))))
