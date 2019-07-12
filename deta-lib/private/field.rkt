#lang racket/base

(require racket/contract
         racket/string)

(provide
 make-field (struct-out field))

(struct field
  (name
   kwd
   type
   getter
   setter
   updater
   primary-key?
   auto-increment?
   nullable?)
  #:transparent)

(define (make-field #:name name
                    #:type type
                    #:getter getter
                    #:setter setter
                    #:updater updater
                    #:primary-key? primary-key?
                    #:auto-increment? auto-increment?
                    #:nullable? nullable?)
  (field (string-replace (symbol->string name) "-" "_")
         (string->keyword (symbol->string name))
         type
         getter
         setter
         updater
         primary-key?
         auto-increment?
         nullable?))
