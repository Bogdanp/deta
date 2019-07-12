#lang at-exp racket/base

(require racket/contract
         racket/format
         racket/match
         racket/port
         racket/string
         "../private/field.rkt"
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

       (define/contract (adapter-last-id-query _)
         (-> adapter? string?)
         "SELECT lastval()")

       (define/contract (adapter-emit-ddl _ d)
         (-> adapter? ddl? string?)
         (emit-ddl d))

       (define/contract (adapter-emit-query _ s)
         (-> adapter? stmt? string?)
         (emit-stmt s))])

    (values postgresql-adapter? (postgresql-adapter))))

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
    [(id/f?          t) "INTEGER"]
    [(integer/f?     t) "INTEGER"]
    [(real/f?        t) "REAL"]
    [(numeric/f?     t) @~a{NUMERIC(@(numeric/f-precision t), @(numeric/f-scale t))}]
    [(string/f?      t) "TEXT"]
    [(binary/f?      t) "BLOB"]
    [(symbol/f?      t) "TEXT"]
    [(boolean/f?     t) "BOOLEAN"]
    [(date/f?        t) "DATE"]
    [(time/f?        t) "TIME"]
    [(datetime/f?    t) "TIMESTAMP"]
    [(datetime-tz/f? t) "TIMESTAMPTZ"]
    [(json/f?        t) "JSON"]
    [(jsonb/f?       t) "JSONB"]
    [else (raise-argument-error 'field-type->postgresql "unsupported type for DDL" t)]))

(define (emit-expr e)
  (match e
    [_ (emit-expr/standard e)]))

(define emit-expr/standard
  (make-expr-emitter emit-expr))

(define (emit-stmt e)
  (match e
    [_ (emit-stmt/standard e)]))

(define emit-stmt/standard
  (make-stmt-emitter emit-stmt emit-expr
                     #:supports-returning? #t))
