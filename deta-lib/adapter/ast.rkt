#lang racket/base

(require racket/match)

(provide
 clause?
 stmt?
 stmt-schema

 (struct-out qualified-name)
 (struct-out alias-expr)
 (struct-out column-expr)
 (struct-out table-expr)
 (struct-out from-clause)
 (struct-out where-clause)
 (struct-out select-stmt)
 (struct-out insert-stmt))

(struct clause ()
  #:transparent)

(struct stmt ()
  #:transparent)

(define (stmt-schema e)
  (match e
    [(select-stmt (from-clause schema _) _ _) schema]
    [_ #f]))

(struct qualified-name (parent name)
  #:transparent)

(struct alias-expr (e alias)
  #:transparent)

(struct column-expr (e)
  #:transparent)

(struct table-expr (e)
  #:transparent)

(struct from-clause (schema table)
  #:transparent)

(struct where-clause (e)
  #:transparent)

(struct select-stmt stmt (from columns where)
  #:transparent)

(struct insert-stmt stmt (into columns returning)
  #:transparent)
