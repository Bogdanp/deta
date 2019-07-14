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


;; expr ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 expr?
 expr-terminal?

 (struct-out app)
 (struct-out as)
 (struct-out case-e)
 (struct-out column)
 (struct-out name)
 (struct-out placeholder)
 (struct-out qualified)
 (struct-out scalar)
 (struct-out table))

(struct expr ()
  #:transparent)

(define (expr-terminal? e)
  (not (app? e)))

(struct app expr (f args)
  #:transparent)

(struct as expr (e alias)
  #:transparent)

(struct case-e expr (clauses else-clause)
  #:transparent)

(struct column expr (e)
  #:transparent)

(struct name expr (name)
  #:transparent)

(struct placeholder expr (v)
  #:transparent)

(struct qualified expr (parent name)
  #:transparent)

(struct scalar expr (v)
  #:transparent)

(struct table expr (e)
  #:transparent)


;; clauses ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 clause?
 (struct-out assignments)
 (struct-out from)
 (struct-out group-by)
 (struct-out limit)
 (struct-out offset)
 (struct-out order-by)
 (struct-out where))

(struct clause ()
  #:transparent)

(struct assignments clause (pairs)
  #:transparent)

(struct from clause (e)
  #:transparent)

(struct group-by clause (columns)
  #:transparent)

(struct limit clause (n)
  #:transparent)

(struct offset clause (n)
  #:transparent)

(struct order-by clause (pairs)
  #:transparent)

(struct where clause (e)
  #:transparent)


;; statements ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 stmt?
 (struct-out delete)
 (struct-out insert)

 make-update
 (struct-out update)

 make-select
 (struct-out select))

(struct stmt ()
  #:transparent)

(struct delete stmt (from where)
  #:transparent)

(struct insert stmt (into columns column-values returning)
  #:transparent)

(struct update stmt (table assignments where)
  #:transparent)

(define (make-update #:table table
                     #:assignments assignments
                     #:where [where #f])
  (update table assignments where))

(struct select stmt (columns from where group-by order-by offset limit)
  #:transparent)

(define (make-select #:columns [columns null]
                     #:from [from #f]
                     #:where [where #f]
                     #:group-by [group-by #f]
                     #:order-by [order-by #f]
                     #:offset [offset #f]
                     #:limit [limit #f])
  (select columns from where group-by order-by offset limit))
