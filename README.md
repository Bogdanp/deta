# deta

A *WIP* functional database mapper for Racket.

This is currently alpha stuff and you should not use it.

## Example

```racket
#lang racket

(require db
         deta
         gregor
         threading)

(define conn
  (sqlite3-connect #:database 'memory))

(define-schema post
  ([id id/f #:primary-key #:auto-increment]
   [title string/f]
   [(slug (slugify title)) string/f #:unique]
   [content string/f]
   [(created-at (now/moment)) datetime-tz/f]
   [(updated-at (now/moment)) datetime-tz/f]
   [published-at datetime-tz/f #:nullable]))

(create-table! conn 'post)

(define my-first-post
  (car
   (insert! conn (make-post #:title "My First Post"
                            #:content "Hello, world!"))))

(for ([p (in-rows (select (from 'post)))])
  (displayln p))
```
