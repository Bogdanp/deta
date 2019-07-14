#lang at-exp racket/base

(require racket/contract
         racket/format
         racket/match
         racket/port
         racket/string
         "../private/ast.rkt"
         "../private/field.rkt"
         "../type.rkt"
         "adapter.rkt"
         "standard.rkt")

(provide
 sqlite3-adapter?
 sqlite3-adapter)

(define-values (sqlite3-adapter? sqlite3-adapter)
  (let ()
    (struct sqlite3-adapter ()
      #:methods gen:adapter
      [(define (adapter-supports-returning? _) #f)

       (define/contract (adapter-last-id-query _)
         (-> adapter? string?)
         "SELECT last_insert_rowid()")

       (define/contract (adapter-emit-ddl _ d)
         (-> adapter? ddl? string?)
         (emit-ddl d))

       (define/contract (adapter-emit-query/impl _ s)
         (-> adapter? stmt? string?)
         (emit-stmt s))])

    (values sqlite3-adapter? (sqlite3-adapter))))

(define (emit-ddl d)
  (match d
    [(create-table-ddl table fields)
     (with-output-to-string
       (lambda _
         (displayln (~a "CREATE TABLE IF NOT EXISTS " (quote/standard table) "("))
         (displayln (string-join (map emit-field-ddl fields) ",\n"))
         (displayln ")")))]

    [(drop-table-ddl table)
     (~a "DROP TABLE IF EXISTS " (quote/standard table))]))

(define (emit-field-ddl f)
  (with-output-to-string
    (lambda _
      (display (~a (quote/standard (field-name f)) " " (field-type->sqlite3 (field-type f))))
      (unless (field-nullable? f) (display " NOT NULL"))
      (when (field-primary-key? f) (display " PRIMARY KEY"))
      (when (field-auto-increment? f) (display " AUTOINCREMENT"))
      (when (field-unique? f) (display " UNIQUE")))))

(define (field-type->sqlite3 t)
  (cond
    [(id/f?          t) "INTEGER"]
    [(integer/f?     t) "INTEGER"]
    [(real/f?        t) "REAL"]
    [(string/f?      t) "TEXT"]
    [(binary/f?      t) "BLOB"]
    [(symbol/f?      t) "TEXT"]
    [else (raise-argument-error 'field-type->sqlite3 "unsupported type for DDL" t)]))

(define (emit-expr e)
  (match e
    [_ (emit-expr/standard e)]))

(define emit-expr/standard
  (make-expr-emitter emit-expr))

(define (emit-stmt e)
  (match e
    [_ (emit-stmt/standard e)]))

(define emit-stmt/standard
  (make-stmt-emitter emit-stmt emit-expr))
