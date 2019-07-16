#lang at-exp racket/base

(require racket/contract
         racket/format
         racket/match
         racket/port
         racket/string
         "../ast.rkt"
         "../field.rkt"
         "../type.rkt"
         "dialect.rkt"
         "standard.rkt")

(provide
 sqlite3-dialect?
 sqlite3-dialect)

(define-values (sqlite3-dialect? sqlite3-dialect)
  (let ()
    (struct sqlite3-dialect ()
      #:methods gen:dialect
      [(define (dialect-name _) 'sqlite3)
       (define (dialect-supports-returning? _) #f)

       (define/contract (dialect-last-id-query _)
         (-> dialect? string?)
         "SELECT last_insert_rowid()")

       (define/contract (dialect-emit-ddl _ d)
         (-> dialect? ddl? string?)
         (emit-ddl d))

       (define/contract (dialect-emit-query/impl _ s)
         (-> dialect? stmt? string?)
         (emit-stmt s))])

    (values sqlite3-dialect? (sqlite3-dialect))))

(define (emit-ddl d)
  (match d
    [(create-table table fields)
     (with-output-to-string
       (lambda _
         (displayln (~a "CREATE TABLE IF NOT EXISTS " (quote/standard table) "("))
         (displayln (string-join (map emit-field-ddl fields) ",\n"))
         (displayln ")")))]

    [(drop-table table)
     (~a "DROP TABLE IF EXISTS " (quote/standard table))]))

(define (emit-field-ddl f)
  (with-output-to-string
    (lambda _
      (display (~a (quote/standard (field-name f)) " " (type-declaration (field-type f) 'sqlite3)))
      (unless (field-nullable? f) (display " NOT NULL"))
      (when (field-primary-key? f) (display " PRIMARY KEY"))
      (when (field-auto-increment? f) (display " AUTOINCREMENT"))
      (when (field-unique? f) (display " UNIQUE")))))

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
