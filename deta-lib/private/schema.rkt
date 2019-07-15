#lang racket/base

(require racket/contract
         racket/match
         "field.rkt")

;; struct ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-schema
 schema?
 schema-name
 schema-table-name
 schema-virtual?
 schema-struct-ctor
 schema-struct-pred
 schema-meta-updater
 schema-fields
 schema-primary-key)

(struct schema
  (name
   table-name
   virtual?
   struct-ctor
   struct-pred
   meta-updater
   fields)
  #:transparent)

(define (make-schema #:name name
                     #:table-name table-name
                     #:virtual? virtual?
                     #:struct-ctor struct-ctor
                     #:struct-pred struct-pred
                     #:meta-updater meta-updater
                     #:fields fields)

  (define the-schema
    (schema name
            table-name
            virtual?
            struct-ctor
            struct-pred
            meta-updater
            fields))

  (begin0 the-schema
    (unless virtual?
      (register! name the-schema))))

(define/contract (schema-primary-key schema)
  (-> schema? (or/c false/c field?))
  (for/first ([f (in-list (schema-fields schema))]
              #:when (field-primary-key? f))
    f))


;; registry ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 schema-registry-ref
 schema-registry-lookup)

(define/contract schema-registry
  (parameter/c (hash/c symbol? schema?))
  (make-parameter (hasheq)))

(define (register! name schema)
  (define registry (schema-registry))
  (when (hash-has-key? registry name)
    (raise-user-error 'register! "schema ~a conflicts with a previous one" name))

  (schema-registry
   (hash-set registry name schema)))

(define/contract (schema-registry-ref name)
  (-> symbol? (or/c false/c schema?))
  (hash-ref (schema-registry) name #f))

(define/contract (schema-registry-lookup schema-or-name)
  (-> (or/c schema? symbol?) schema?)
  (define schema
    (match schema-or-name
      [(? schema?)                      schema-or-name ]
      [(? symbol?) (schema-registry-ref schema-or-name)]))

  (unless schema
    (raise-argument-error 'lookup-schema "unregistered schema" schema-or-name))

  schema)
