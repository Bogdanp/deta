#lang racket/base

(require racket/match)

(provide
 make-meta
 meta?
 meta-state
 meta-schema
 meta-can-persist?
 meta-can-delete?
 meta-track-change
 meta-track-persisted
 meta-track-deleted)

(struct meta (state schema)
  #:transparent)

(define (make-meta schema)
  (meta 'created schema))

(define (meta-track-change m)
  (struct-copy meta m [state (match (meta-state m)
                               ['created   'created]
                               ['persisted 'changed]
                               ['changed   'changed]
                               ['deleted   'deleted])]))

(define (meta-can-persist? meta)
  (match (meta-state meta)
    ['created #t]
    ['changed #t]
    [_        #f]))

(define (meta-can-delete? meta)
  (match (meta-state meta)
    ['persisted #t]
    ['changed   #t]
    [_          #f]))

(define (meta-track-persisted m)
  (struct-copy meta m [state 'persisted]))

(define (meta-track-deleted m)
  (struct-copy meta m [state 'deleted]))
