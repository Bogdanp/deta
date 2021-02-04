#lang racket/base

(require racket/contract
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

  (define pk-fields
    (filter field-primary-key? fields))
  (when (> (length pk-fields) 1)
    (raise-arguments-error 'make-schema
                           "at most one field may be marked as a #:primary-key"
                           "bad fields" (map field-id pk-fields)))

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
            (findf field-primary-key? pk-fields)))

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

(define (register! id s)
  (define registry (schema-registry))
  (when (hash-has-key? registry id)
    (raise-user-error 'register! "schema ~a conflicts with a previous one" id))

  (schema-registry
   (hash-set registry id s)))

(define/contract (schema-registry-lookup schema-or-id)
  (-> (or/c schema? symbol?) schema?)
  (cond
    [(schema? schema-or-id) schema-or-id]
    [else (hash-ref
           (schema-registry)
           schema-or-id
           (lambda ()
             (raise-user-error 'lookup-schema "unregistered schema~n  id: ~s" schema-or-id)))]))
