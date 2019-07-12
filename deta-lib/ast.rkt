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

 (struct-out qualified)
 (struct-out as)
 (struct-out binop)
 (struct-out column)
 (struct-out placeholder)
 (struct-out table)
 (struct-out from)
 (struct-out where)
 (struct-out insert)
 (struct-out delete)
 (struct-out select))

(struct expr ()
  #:transparent)

(struct clause ()
  #:transparent)

(struct stmt ()
  #:transparent)

(struct qualified expr (parent name)
  #:transparent)

(struct as expr (e alias)
  #:transparent)

(struct binop expr (op a b)
  #:transparent)

(struct column expr (e)
  #:transparent)

(struct placeholder expr (n)
  #:transparent)

(struct table expr (e)
  #:transparent)

(struct from clause (e)
  #:transparent)

(struct where clause (e)
  #:transparent)

(struct insert stmt (into columns column-values returning)
  #:transparent)

(struct delete stmt (from where)
  #:transparent)

(struct select stmt (from columns where)
  #:transparent)
