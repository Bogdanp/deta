#lang racket/base

(require racket/contract
         racket/match
         "field.rkt")

;; struct ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-schema
 (struct-out schema))

(struct schema
  (id
   table
   virtual?
   struct-ctor
   struct-pred
   meta-updater
   fields
   primary-key)
  #:transparent)

(define (make-schema #:id id
                     #:table table
                     #:virtual? virtual?
                     #:struct-ctor struct-ctor
                     #:struct-pred struct-pred
                     #:meta-updater meta-updater
                     #:fields fields)

  (define the-schema
    (schema id
            table
            virtual?
            struct-ctor
            struct-pred
            meta-updater
            fields
            (findf field-primary-key? fields)))

  (begin0 the-schema
    (unless virtual?
      (register! id the-schema))))


;; registry ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 schema-registry-lookup)

(define/contract schema-registry
  (parameter/c (hash/c symbol? schema?))
  (make-parameter (hasheq)))

(define (register! id schema)
  (define registry (schema-registry))
  (when (hash-has-key? registry id)
    (raise-user-error 'register! "schema ~a conflicts with a previous one" id))

  (schema-registry
   (hash-set registry id schema)))

(define/contract (schema-registry-lookup schema-or-id)
  (-> (or/c schema? symbol?) schema?)
  (define schema
    (match schema-or-id
      [(? schema?)                             schema-or-id ]
      [(? symbol?) (hash-ref (schema-registry) schema-or-id)]))

  (unless schema
    (raise-argument-error 'lookup-schema "unregistered schema" schema-or-id))

  schema)
