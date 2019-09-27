#lang racket/base

;; ddl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 ddl?

 (struct-out create-table)
 (struct-out drop-table))

(struct ddl ()
  #:transparent)

;; TODO: Eventually there should be a ddl-field struct.
(struct create-table ddl (table fields)
  #:transparent)

(struct drop-table ddl (table)
  #:transparent)


;; expr ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 expr?
 expr-terminal?

 (struct-out app)
 (struct-out as)
 (struct-out case-e)
 (struct-out column)
 (struct-out ident)
 (struct-out placeholder)
 (struct-out qualified)
 (struct-out scalar)
 (struct-out subquery)
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

(struct ident expr (name)
  #:transparent)

(struct placeholder expr (v)
  #:transparent)

(struct qualified expr (parent name)
  #:transparent)

(struct scalar expr (v)
  #:transparent)

(struct subquery expr (stmt)
  #:transparent)

(struct table expr (e)
  #:transparent)


;; clauses ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 clause?
 (struct-out assignments)

 make-from
 (struct-out from)

 (struct-out group-by)
 (struct-out join)
 (struct-out limit)
 (struct-out offset)
 (struct-out union)
 (struct-out order-by)
 (struct-out returning)
 (struct-out where))

(struct clause ()
  #:transparent)

(struct assignments clause (pairs)
  #:transparent)

(struct from clause (tables joins)
  #:transparent)

(define (make-from #:tables tables
                   #:joins [joins null])
  (from tables joins))

(struct group-by clause (columns)
  #:transparent)

(struct join clause (type with constraint)
  #:transparent)

(struct limit clause (n)
  #:transparent)

(struct offset clause (n)
  #:transparent)

(struct union clause (stmt)
  #:transparent)

(struct order-by clause (pairs)
  #:transparent)

(struct returning clause (es)
  #:transparent)

(struct where clause (e)
  #:transparent)


;; statements ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 stmt?

 make-delete
 (struct-out delete)

 make-insert
 (struct-out insert)

 make-update
 (struct-out update)

 make-select
 (struct-out select))

(struct stmt ()
  #:transparent)

(struct delete stmt (from where returning)
  #:transparent)

(define (make-delete #:from from
                     #:where [where #f]
                     #:returning [returning #f])
  (delete from where returning))

(struct insert stmt (into columns column-values returning)
  #:transparent)

(define (make-insert #:into into
                     #:columns columns
                     #:values column-values
                     #:returning [returning #f])
  (insert into columns column-values returning))

(struct update stmt (table assignments where returning)
  #:transparent)

(define (make-update #:table table
                     #:assignments assignments
                     #:where [where #f]
                     #:returning [returning #f])
  (update table assignments where returning))

(struct select stmt (columns from where group-by union order-by offset limit)
  #:transparent)

(define (make-select #:columns [columns null]
                     #:from [from #f]
                     #:where [where #f]
                     #:group-by [group-by #f]
                     #:union [union #f]
                     #:order-by [order-by #f]
                     #:offset [offset #f]
                     #:limit [limit #f])
  (select columns from where group-by union order-by offset limit))
