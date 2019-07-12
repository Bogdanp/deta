#lang racket/base

(require racket/match)


;; ddl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 ddl?

 (struct-out create-table-ddl)
 (struct-out drop-table-ddl))

(struct ddl ()
  #:transparent)

;; TODO: Eventually there should be a ddl-field struct.
(struct create-table-ddl ddl (table fields)
  #:transparent)

(struct drop-table-ddl ddl (table)
  #:transparent)


;; stmt ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 stmt?
 stmt-schema

 (struct-out qualified-name)
 (struct-out alias-expr)
 (struct-out binary-expr)
 (struct-out column-expr)
 (struct-out placeholder-expr)
 (struct-out table-expr)
 (struct-out from-clause)
 (struct-out where-clause)
 (struct-out insert-stmt)
 (struct-out delete-stmt)
 (struct-out select-stmt))

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

(struct binary-expr (op a b)
  #:transparent)

(struct column-expr (e)
  #:transparent)

(struct placeholder-expr (n)
  #:transparent)

(struct table-expr (e)
  #:transparent)

(struct from-clause (schema table)
  #:transparent)

(struct where-clause (e)
  #:transparent)

(struct insert-stmt stmt (into columns column-values returning)
  #:transparent)

(struct delete-stmt stmt (from where)
  #:transparent)

(struct select-stmt stmt (from columns where)
  #:transparent)
