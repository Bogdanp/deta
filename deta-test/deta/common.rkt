#lang racket/base

(require deta
         gregor
         racket/string)

(provide
 deleted-users
 (schema-out book-with-nulls)
 (schema-out user)
 (schema-out password-reset))

(define (generate-random-string)
  "a random string -- I promise")

(define-schema book-with-nulls
  #:table "book_with_nulls"
  ([id id/f #:primary-key #:auto-increment]
   [title string/f #:contract non-empty-string? #:wrapper string-titlecase]
   [published-on datetime/f #:nullable]))

(define deleted-users
  (make-parameter null))

(define-schema user
  ([id id/f #:primary-key #:auto-increment]
   [username string/f #:unique]
   [(active? #f) boolean/f]
   [(password-hash "") string/f]
   [(verified? #f) boolean/f #:name "verified"]
   [(verification-code (generate-random-string)) string/f]
   [(created-at (now/moment)) datetime-tz/f]
   [(updated-at (now/moment)) datetime-tz/f])

  #:pre-persist-hook
  (lambda (u)
    (set-user-updated-at u (now/moment)))

  #:pre-delete-hook
  (lambda (u)
    (begin0 u
      (deleted-users (cons (user-id u) (deleted-users))))))

(define-schema password-reset
  #:table "password_reset_tokens"
  ([user-id id/f #:unique]
   [(token (generate-random-string)) string/f]
   [(expires-at (+days (now/moment) 1)) datetime-tz/f]))
