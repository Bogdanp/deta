#lang racket/base

(require deta
         gregor)

(provide
 (schema-out user)
 (schema-out password-reset))

(define (generate-random-string)
  "a random string -- I promise")

(define-schema user
  ([id id/f #:primary-key #:auto-increment]
   [username string/f #:unique]
   [(active? #f) boolean/f]
   [(password-hash "") string/f]
   [(verified? #f) boolean/f #:name "verified"]
   [(verification-code (generate-random-string)) string/f]
   [(created-at (now/moment)) datetime-tz/f]
   [(updated-at (now/moment)) datetime-tz/f]))

(define-schema password-reset
  #:table "password_reset_tokens"
  ([user-id id/f #:unique]
   [(token (generate-random-string)) string/f]
   [(expires-at (+days (now/moment) 1)) datetime-tz/f]))
