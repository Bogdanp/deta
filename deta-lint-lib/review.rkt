#lang racket/base

(require review/ext
         syntax/parse/pre)

#|review: ignore|#

(provide
 should-review-syntax?
 review-syntax)

(define (should-review-syntax? stx)
  (syntax-case stx (define-schema define-type)
    [(define-schema . _rest) #t]
    [(define-type . _rest) #t]
    [_ #f]))

(define-splicing-syntax-class schema-field-option
  (pattern {~seq {~alt
                  {~optional #:primary-key}
                  {~optional #:auto-increment}
                  {~optional #:nullable}
                  {~optional #:unique}
                  {~optional #:virtual}
                  {~optional {~seq #:name name-e:expression}}
                  {~optional {~seq #:contract contract-e:expression}}
                  {~optional {~seq #:wrapper wrapper-e:expression}}} ...}))

(define-syntax-class schema-field
  (pattern [id:id type-expr:expression opt:schema-field-option])
  (pattern [(id:id default-expr:expression) type-expr:expression opt:schema-field-option])
  (pattern e
           #:with id #'invalid
           #:do [(track-error this-syntax "expected a valid deta field definition")]))

(define-syntax-class schema-definition
  #:datum-literals (define-schema)
  (pattern (define-schema
             ~!
             schema-id:id
             {~do (push-scope)}
             {~alt {~optional {~seq #:table table-name:str}}
                   {~optional #:virtual}} ...
             (schema-field:schema-field ...+)
             {~alt {~optional {~seq #:pre-persist-hook pre-persist-hook-expr:expression}}
                   {~optional {~seq #:pre-delete-hook pre-delete-hook-expr:expression}}} ...
             struct-option ...
             {~do (pop-scope)})
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

(define-syntax-class type-definition
  #:datum-literals (define-type)
  (pattern (define-type
             ~!
             type-id:id
             {~do (push-scope)}
             {~optional (field-id:id ...)}
             {~alt {~optional {~seq {~or #:contract
                                         #:contract-fn}
                                    contract-expr:expression}}
                   {~optional {~seq #:declaration declaration-expr:expression}}
                   {~optional {~seq #:constructor constructor-expr:expression}}
                   {~optional {~seq #:dump dump-expr:expression}}
                   {~optional {~seq #:load load-expr:expression}}} ...)
           #:do [(pop-scope)
                 (track-binding #'type-id "~a/f" #:check-usages? #f)
                 (track-binding #'type-id "~a/f?" #:check-usages? #f)]))

(define (review-syntax stx)
  (syntax-parse stx
    [d:schema-definition #'d]
    [t:type-definition #'t]
    [_ (track-error stx "expected a deta schema or type definition")]))
