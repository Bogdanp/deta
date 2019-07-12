#lang at-exp racket/base

(require racket/contract
         racket/format
         racket/list
         racket/match
         racket/port
         racket/string
         "../private/field.rkt"
         "../schema.rkt"
         "../type.rkt"
         "adapter.rkt"
         "ast.rkt"
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

       (define/contract (adapter-emit-ddl _ s)
         (-> adapter? schema? string?)
         (with-output-to-string
           (lambda _
             (displayln (~a "CREATE TABLE IF NOT EXISTS " (quote/standard (schema-table-name s)) "("))
             (displayln (string-join (map emit-field-ddl (schema-fields s)) ",\n"))
             (displayln ")"))))

       (define/contract (adapter-emit-query _ stmt)
         (-> adapter? stmt? string?)
         (emit/standard stmt))])

    (values sqlite3-adapter?
            (sqlite3-adapter))))

(define (emit-field-ddl f)
  (with-output-to-string
    (lambda _
      (display (~a (quote/standard (field-name f)) " " (field-type->sqlite3 (field-type f))))
      (unless (field-nullable? f) (display " NOT NULL"))
      (when (field-primary-key? f) (display " PRIMARY KEY"))
      (when (field-auto-increment? f) (display " AUTOINCREMENT")))))

(define (field-type->sqlite3 t)
  (cond
    [(id/f? t)      "INTEGER"]
    [(integer/f? t) "INTEGER"]
    [(string/f? t)  "TEXT"]
    [(symbol/f? t)  "TEXT"]
    [(boolean/f? t) "BOOLEAN"]
    [else (raise-argument-error 'field-type->sqlite3 "unsupported type for DDL" t)]))
