#lang racket/base

(require deta
         gregor)

(provide
 (schema-out user))

(define-schema user
  ([id id/f #:primary-key #:auto-increment]
   [username string/f #:unique]
   [(active? #f) boolean/f]
   [password-hash string/f #:nullable]
   [(created-at (now/moment)) datetime-tz/f]
   [(updated-at (now/moment)) datetime-tz/f]))
