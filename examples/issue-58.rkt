#lang racket/base

(require db
         deta
         gregor
         threading)

(define conn
  (postgresql-connect
   #:database "deta_examples"
   #:user "deta"
   #:password "deta"))

(define-schema reserved
  #:table "reserved"
  ([user string/f]
   [(timestamp (now/moment)) datetime-tz/f]))

(create-all! conn)
(query-exec conn "TRUNCATE reserved")

(define entities
  (insert!
   conn
   (make-reserved #:user "example1")
   (make-reserved #:user "example2")))

(query-value conn (select _ user))
(query-value conn (select _ current_timestamp))
(for/list ([r (in-entities conn (from reserved #:as r))]) r)
(for/list ([r (in-entities conn (~> (from reserved #:as r)
                                    (where (= r.user "example1"))))])
  r)
