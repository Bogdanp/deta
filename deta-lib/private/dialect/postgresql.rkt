#lang racket/base

(require db
         racket/contract
         racket/match
         racket/port
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
         (emit-stmt s))

       (define/contract (dialect-prepare-parameters _ p args)
         (-> dialect? prepared-statement? (listof any/c) (listof any/c))
         args)])

    (values postgresql-dialect? (postgresql-dialect))))

(define (emit-ddl d)
  (with-output-to-string
    (lambda ()
      (match d
        [(create-table table fields)
         (display "CREATE TABLE IF NOT EXISTS ")
         (display (quote/standard table))

         (displayln "(")
         (for ([i (in-naturals 1)]
               [f (in-list fields)])
           (emit-field-ddl f (= i 1)))
         (for ([i (in-naturals 1)]
               [f (in-list fields)])
           (emit-field-constraints f))
         (displayln ")")]

        [(drop-table table)
         (display "DROP TABLE IF EXISTS ")
         (displayln (quote/standard table))]))))

(define (emit-field-ddl f first?)
  (unless first?
    (display ","))
  (define type
    (if (field-auto-increment? f)
        "SERIAL"
        (type-declaration (field-type f) 'postgresql)))

  (display (quote/standard (field-name f)))
  (display " ")
  (display type)

  (unless (field-nullable? f)
    (display " NOT NULL"))

  (when (field-primary-key? f)
    (display " PRIMARY KEY"))

  (when (field-unique? f)
    (display " UNIQUE")))

(define (emit-field-constraints f)
  (display ",")
  (when (field-foreign-key? f)
    (display " FOREIGN KEY")
    (display "(")
    (display (quote/standard (field-name f)))
    (display ")")
    (display " REFERENCES ")
    (define fk (field-foreign-key f))
    (display (quote/standard (foreign-key-schema fk)))
    (display "(")
    (display (quote/standard (foreign-key-field fk)))
    (display ")")))

(define (emit-expr e)
  (emit-expr/standard e))

(define (emit-stmt e)
  (with-output-to-string
    (lambda ()
      (emit-stmt/postgresql e))))

(define (emit-stmt/postgresql e)
  (emit-stmt/standard e))

(define emit-expr/standard
  (make-expr-emitter
   emit-expr
   emit-stmt/postgresql))

(define emit-stmt/standard
  (make-stmt-emitter
   #:supports-returning? #t
   emit-stmt/postgresql emit-expr))
