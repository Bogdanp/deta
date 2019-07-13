#lang racket/base

(require racket/match)

(provide
 make-meta
 meta?
 meta-state
 meta-schema
 meta-changes
 meta-can-persist?
 meta-can-update?
 meta-can-delete?
 meta-track-change
 meta-track-persisted
 meta-track-deleted)

(struct meta (state schema changes)
  #:transparent)

(define (make-meta schema)
  (meta 'created schema null))

(define (meta-track-change m f)
  (struct-copy meta m
               [state (match (meta-state m)
                        ['created   'created]
                        ['persisted 'changed]
                        ['changed   'changed]
                        ['deleted   'deleted])]
               [changes (cons f (meta-changes m))]))

(define (meta-can-persist? meta)
  (match (meta-state meta)
    ['created #t]
    ['changed #t]
    [_        #f]))

(define (meta-can-update? meta)
  (match (meta-state meta)
    ['changed #t]
    [_        #f]))

(define (meta-can-delete? meta)
  (match (meta-state meta)
    ['persisted #t]
    ['changed   #t]
    [_          #f]))

(define (meta-track-persisted m)
  (struct-copy meta m
               [state 'persisted]
               [changes null]))

(define (meta-track-deleted m)
  (struct-copy meta m
               [state 'deleted]
               [changes null]))
