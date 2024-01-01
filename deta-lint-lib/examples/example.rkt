#lang racket/base

(require deta)

(define-schema person
  #:table "people"
  ([id id/f]
   [name string/f]
   [(has-pet? #f) boolean/f]))
