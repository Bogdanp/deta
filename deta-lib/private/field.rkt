#lang racket/base

(require racket/format
         racket/string)

(provide
 make-field
 (struct-out field)

 make-foreign-key
 (struct-out foreign-key)

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
   foreign-key?
   foreign-key
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
                    #:foreign-key? foreign-key?
                    #:foreign-key foreign-key
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
         foreign-key?
         foreign-key
         auto-increment?
         nullable?
         unique?
         virtual?))

(struct foreign-key
  (schema
   field))

(define (make-foreign-key #:schema schema
                          #:field field)
  (foreign-key schema field))

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
