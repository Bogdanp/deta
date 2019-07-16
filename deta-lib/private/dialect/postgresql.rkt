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
 postgresql-dialect?
 postgresql-dialect)

(define-values (postgresql-dialect? postgresql-dialect)
  (let ()
    (struct postgresql-dialect ()
      #:methods gen:dialect
      [(define (dialect-name _) 'postgresql)
       (define (dialect-supports-returning? _) #t)

       (define/contract (dialect-last-id-query _)
         (-> dialect? string?)
         "SELECT lastval()")

       (define/contract (dialect-emit-ddl _ d)
         (-> dialect? ddl? string?)
         (emit-ddl d))

       (define/contract (dialect-emit-query/impl _ s)
         (-> dialect? stmt? string?)
         (emit-stmt s))])

    (values postgresql-dialect? (postgresql-dialect))))

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
      (define type
        (if (field-auto-increment? f)
            "SERIAL"
            (type-declaration (field-type f) 'postgresql)))

      (display (~a (quote/standard (field-name f)) " " type))
      (unless (field-nullable? f) (display " NOT NULL"))
      (when (field-primary-key? f) (display " PRIMARY KEY"))
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
  (make-stmt-emitter emit-stmt emit-expr
                     #:supports-returning? #t))
