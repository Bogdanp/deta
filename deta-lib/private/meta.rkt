#lang racket/base

(require racket/set)

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

(struct meta (state schema changes))

(define (make-meta schema)
  (meta 'created schema (seteq)))

(define (meta-track-change m f)
  (struct-copy meta m
               [state (case (meta-state m)
                        [(created)           'created]
                        [(persisted changed) 'changed]
                        [(deleted)           'deleted])]
               [changes (set-add (meta-changes m) f)]))

(define (meta-can-persist? m)
  (case (meta-state m)
    [(created) #t]
    [(changed) #t]
    [else      #f]))

(define (meta-can-update? m)
  (case (meta-state m)
    [(changed) #t]
    [else      #f]))

(define (meta-can-delete? m)
  (case (meta-state m)
    [(persisted) #t]
    [(changed)   #t]
    [else        #f]))

(define (meta-track-persisted m)
  (struct-copy meta m
               [state 'persisted]
               [changes (seteq)]))

(define (meta-track-deleted m)
  (struct-copy meta m
               [state 'deleted]
               [changes (seteq)]))
