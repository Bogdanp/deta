#lang racket/base

(require db
         deta
         threading)

(define-schema example
  ([id id/f #:primary-key #:auto-increment]
   [data json/f]))

(define conn
  (sqlite3-connect #:database 'memory))

(void
 (create-all! conn)
 (insert! conn
          (make-example #:data (hasheq))
          (make-example #:data (hasheq 'hello "world"))))

(displayln "All rows:")
(for ([e (in-entities conn (from example #:as e))])
  (println e))

(displayln "Rows with a 'hello' key:")
(for ([e (in-entities conn (~> (from example #:as e)
                               (where (not (is (json-ref e.data "hello") null)))))])
  (println e))
