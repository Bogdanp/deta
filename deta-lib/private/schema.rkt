#lang racket/base

(require racket/contract
         racket/match
         "field.rkt")

;; struct ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (struct-out schema)
 make-schema
 schema-fields/nonvirtual)

(struct schema
  (id
   table
   virtual?
   struct-ctor
   struct-pred
   meta-updater
   pre-persist-hook
   pre-delete-hook
   fields
   primary-key))

(define (make-schema #:id id
                     #:table table
                     #:virtual? virtual?
                     #:struct-ctor struct-ctor
                     #:struct-pred struct-pred
                     #:meta-updater meta-updater
                     #:pre-persist-hook pre-persist-hook
                     #:pre-delete-hook pre-delete-hook
                     #:fields fields)

  (define the-schema
    (schema id
            table
            virtual?
            struct-ctor
            struct-pred
            meta-updater
            pre-persist-hook
            pre-delete-hook
            fields
            (findf field-primary-key? fields)))

  (begin0 the-schema
    (unless virtual?
      (register! id the-schema))))

(define/contract (schema-fields/nonvirtual the-schema)
  (-> schema? (listof field?))
  (filter (compose1 not field-virtual?)
          (schema-fields the-schema)))

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
      [(? schema?)                             schema-or-id    ]
      [(? symbol?) (hash-ref (schema-registry) schema-or-id #f)]))

  (unless schema
    (raise-argument-error 'lookup-schema "unregistered schema" schema-or-id))

  schema)
