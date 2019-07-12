#lang at-exp racket/base

(require racket/contract
         racket/format
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
 postgresql-adapter?
 postgresql-adapter)

(define-values (postgresql-adapter? postgresql-adapter)
  (let ()
    (struct postgresql-adapter ()
      #:methods gen:adapter
      [(define (adapter-supports-returning? _) #t)

       (define/contract (adapter-emit-ddl _ d)
         (-> adapter? ddl? string?)
         (emit-ddl d))

       (define/contract (adapter-emit-query _ stmt)
         (-> adapter? stmt? string?)
         (emit-query/standard #:supports-returning? #t stmt))])

    (values postgresql-adapter?
            (postgresql-adapter))))

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
      (define type
        (if (field-auto-increment? f)
            "SERIAL"
            (field-type->postgresql (field-type f))))

      (display (~a (quote/standard (field-name f)) " " type))
      (unless (field-nullable? f) (display " NOT NULL"))
      (when (field-primary-key? f) (display " PRIMARY KEY"))
      (when (field-unique? f) (display " UNIQUE")))))

(define (field-type->postgresql t)
  (cond
    [(id/f? t)      "INTEGER"]
    [(integer/f? t) "INTEGER"]
    [(string/f? t)  "TEXT"]
    [(symbol/f? t)  "TEXT"]
    [(boolean/f? t) "BOOLEAN"]
    [else (raise-argument-error 'field-type->postgresql "unsupported type for DDL" t)]))
