#lang racket/base

(require racket/set
         "meta.rkt")

(provide
 (struct-out entity)
 entity-changed?)

(struct entity (meta)
  #:transparent)

(define (entity-changed? e)
  (not (set-empty? (meta-changes (entity-meta e)))))
