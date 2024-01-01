#lang racket/base

(require (for-syntax racket/base)
         review/ext
         syntax/parse/pre)

#|review: ignore|#

(provide
 should-review-syntax?
 review-syntax)

(define-expression-syntax-class expression)

(define (should-review-syntax? stx)
  (syntax-case stx (define-schema)
    [(define-schema . _rest) #t]
    [_ #f]))

(define-syntax-class schema-field
  (pattern [id:id type-expr:expression . opts])
  (pattern [(id:id default-expr:expression) type-expr . opts]))

(define-syntax-class schema-definition
  #:datum-literals (define-schema)
  (pattern (define-schema
             ~!
             schema-id:id
             {~alt {~optional {~seq #:table table-name:str}}
                   {~optional #:virtual}} ...
             (schema-field:schema-field ...+)
             {~alt {~optional {~seq #:pre-persist-hook pre-persist-hook-expr:expression}}
                   {~optional {~seq #:pre-delete-hook pre-delete-hook-expr:expression}}} ...
             struct-option ...)
           #:do [(track-binding #'schema-id #:check-usages? #f)
                 (track-binding #'schema-id "~a?" #:check-usages? #f)
                 (track-binding #'schema-id "make-~a" #:check-usages? #f)
                 (define schema-id-sym (syntax->datum #'schema-id))
                 (for ([field-id-stx (in-list (syntax-e #'(schema-field.id ...)))])
                   (for ([p (in-list '("" "set-" "update-"))])
                     (track-binding
                      field-id-stx
                      #:related-to #'schema-id
                      #:check-usages? #f
                      (format "~a~a-~~a" p schema-id-sym))))]))

(define (review-syntax stx)
  (syntax-parse stx
    [d:schema-definition #'d]
    [_ stx]))
