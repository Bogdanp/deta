#lang racket/base

(require racket/contract
         racket/format
         racket/string
         "private/type.rkt")

(provide
 make-field
 (contract-out
  [struct field ([name string?]
                 [kwd keyword?]
                 [type type?]
                 [getter (-> any/c any/c)]
                 [setter (->* (any/c any/c) (boolean?) any/c)]
                 [updater (->* (any/c (-> any/c any/c)) (boolean?) any/c)]
                 [primary-key? boolean?]
                 [auto-increment? boolean?]
                 [nullable? boolean?]
                 [unique? boolean?])])

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

(define/contract (make-field #:name name
                             #:type type
                             #:getter getter
                             #:setter setter
                             #:updater [updater (lambda (e p)
                                                  (raise-user-error 'updater "no updater for ~a" name))]
                             #:primary-key? [primary-key? #f]
                             #:auto-increment? [auto-increment? #f]
                             #:nullable? [nullable? #f]
                             #:unique? [unique? #f])
  (->* (#:name symbol?
        #:type type?
        #:getter (-> any/c any/c)
        #:setter (->* (any/c any/c) (boolean?) any/c))
       (#:updater (->* (any/c (-> any/c any/c)) (boolean?) any/c)
        #:primary-key? boolean?
        #:auto-increment? boolean?
        #:nullable? boolean?
        #:unique? boolean?)
       field?)

  (field (id->column-name name)
         (string->keyword (symbol->string name))
         type
         getter
         setter
         updater
         primary-key?
         auto-increment?
         nullable?
         unique?))

(define/contract (id->column-name id)
  (-> (or/c symbol? string?) string?)
  (let* ([name (cond
                 [(symbol? id) (symbol->string id)]
                 [(string? id) id])]
         [name (string-replace name "-" "_")]
         [name (if (string-suffix? name "?")
                   (~a "is_" (substring name 0 (sub1 (string-length name))))
                   name)])
    name))
