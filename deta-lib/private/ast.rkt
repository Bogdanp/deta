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
 expr?
 stmt?

 (struct-out name)
 (struct-out scalar)
 (struct-out qualified)
 (struct-out as)
 (struct-out app)
 (struct-out column)
 (struct-out placeholder)
 (struct-out table)
 (struct-out assignments)
 (struct-out from)
 (struct-out where)
 (struct-out insert)
 (struct-out update)
 (struct-out delete)
 (struct-out select))

(struct expr ()
  #:transparent)

(struct clause ()
  #:transparent)

(struct stmt ()
  #:transparent)

(struct name expr (name)
  #:transparent)

(struct scalar expr (v)
  #:transparent)

(struct qualified expr (parent name)
  #:transparent)

(struct as expr (e alias)
  #:transparent)

(struct app expr (f args)
  #:transparent)

(struct column expr (e)
  #:transparent)

(struct placeholder expr (v)
  #:transparent)

(struct table expr (e)
  #:transparent)

(struct from clause (e)
  #:transparent)

(struct assignments clause (pairs)
  #:transparent)

(struct where clause (e)
  #:transparent)

(struct insert stmt (into columns column-values returning)
  #:transparent)

(struct update stmt (table assignments where)
  #:transparent)

(struct delete stmt (from where)
  #:transparent)

(struct select stmt (columns from where)
  #:transparent)
