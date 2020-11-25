#lang racket/base

(require racket/format
         racket/string)

(provide
 make-field
 (struct-out field)

 id->column-name)

(struct field
  (id
   name
   kwd
   type
   getter
   setter
   updater
   primary-key?
   auto-increment?
   nullable?
   unique?
   virtual?))

(define (make-field #:id id
                    #:name name
                    #:kwd [kwd (symbol->keyword id)]
                    #:type type
                    #:getter getter
                    #:setter setter
                    #:updater updater
                    #:primary-key? primary-key?
                    #:auto-increment? auto-increment?
                    #:nullable? nullable?
                    #:unique? unique?
                    #:virtual? virtual?)
  (field id
         name
         kwd
         type
         getter
         setter
         updater
         primary-key?
         auto-increment?
         nullable?
         unique?
         virtual?))

(define (id->column-name id)
  (let* ([name (cond
                 [(symbol? id) (symbol->string id)]
                 [(string? id) id])]
         [name (string-replace name "-" "_")]
         [name (if (string-suffix? name "?")
                   (~a "is_" (substring name 0 (sub1 (string-length name))))
                   name)])
    name))

(define symbol->keyword
  (compose1 string->keyword symbol->string))
