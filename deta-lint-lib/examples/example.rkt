#lang racket/base

(require deta)

(define has-pet?-default #f)

(define-schema person
  #:table "people"
  ([id id/f]
   [name string/f]
   [(has-pet? has-pet?-default) boolean/f]))
