#lang racket/base

(require deta
         gregor)

(provide
 make-user
 user?
 user
 user-schema
 user-id
 user-username
 set-user-username
 update-user-username
 user-active?
 user-password-hash
 user-created-at
 user-updated-at)

(define-schema user
  ([id id/f #:primary-key #:auto-increment]
   [username string/f #:unique]
   [(active? #f) boolean/f]
   [password-hash string/f #:nullable]
   [(created-at (now/moment)) datetime-tz/f]
   [(updated-at (now/moment)) datetime-tz/f]))
