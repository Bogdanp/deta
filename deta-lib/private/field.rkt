#lang racket/base

(require racket/format
         racket/string)

(provide
 make-field
 (struct-out field)

 id->column-name)

(struct field
  (name
   kwd
   type
   getter
   setter
   updater
   primary-key?
   auto-increment?
   nullable?
   unique?)
  #:transparent)

(define (make-field #:name name
                    #:type type
                    #:getter getter
                    #:setter setter
                    #:updater updater
                    #:primary-key? primary-key?
                    #:auto-increment? auto-increment?
                    #:nullable? nullable?
                    #:unique? unique?)
  (define name:str (symbol->string name))
  (field (id->column-name name:str)
         (string->keyword name:str)
         type
         getter
         setter
         updater
         primary-key?
         auto-increment?
         nullable?
         unique?))

(define (id->column-name id)
  (let* ([name (cond
                 [(symbol? id) (symbol->string id)]
                 [(string? id) id])]
         [name (string-replace name "-" "_")]
         [name (if (string-suffix? name "?")
                   (~a "is_" (substring name 0 (sub1 (string-length name))))
                   name)])
    name))
