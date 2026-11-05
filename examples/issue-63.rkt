#lang racket/base

(require db
         deta
         racket/contract/base
         threading)

(define conn
  (sqlite3-connect
   #:database 'memory))

(define-schema event
  ([id string/f #:primary-key]
   [learner-id string/f]
   [lesson-id string/f]
   [type string/f #:name "typ" #:contract (or/c "started" "completed")]
   [occurred-at datetime-tz/f]))

(create-all! conn)

(~> (from event #:as evt)
    (where (= evt.type "completed"))
    (query-entities conn _))
