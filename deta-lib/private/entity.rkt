#lang racket/base

(require racket/set
         "field.rkt"
         "meta.rkt"
         "schema.rkt")

(provide
 (struct-out entity)
 entity-changed?)

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

(define (entity-changed? e)
  (not (set-empty? (meta-changes (entity-meta e)))))
