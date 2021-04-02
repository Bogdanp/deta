#lang racket/base

(require racket/set
         "field.rkt"
         "meta.rkt"
         "schema.rkt")

(provide
 (struct-out entity)
 entity-schema
 entity-changed?
 entity->hash)

(define (entity=? a b recur)
  (and (entity? b)
       (eq? (meta-schema (entity-meta a))
            (meta-schema (entity-meta b)))
       (for/and ([fld (in-list (schema-fields (meta-schema (entity-meta a))))])
         (recur ((field-getter fld) a)
                ((field-getter fld) b)))))

(define (entity-hash-code e recur)
  (define m (entity-meta e))
  (define s (meta-schema m))
  (for/fold ([code (recur s)])
            ([fld (in-list (schema-fields s))])
    (bitwise-xor code
                 (recur (field-id fld))
                 (recur ((field-getter fld) e)))))

(struct entity (meta)
  #:transparent
  #:methods gen:equal+hash
  [(define equal-proc entity=?)
   (define hash-proc  entity-hash-code)
   (define hash2-proc entity-hash-code)])

(define (entity-schema e)
  (meta-schema (entity-meta e)))

(define (entity-changed? e)
  (not (set-empty? (meta-changes (entity-meta e)))))

(define (entity->hash e [f (λ (_k v) v)])
  (for*/hasheq ([fld (in-list (schema-fields (entity-schema e)))]
                [key (in-value (field-id fld))]
                [val (in-value (f key ((field-getter fld) e)))])
    (values key val)))
