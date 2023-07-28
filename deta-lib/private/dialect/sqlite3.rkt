#lang racket/base

(require db
         racket/contract
         racket/match
         racket/port
         "../ast.rkt"
         "../field.rkt"
         "../type.rkt"
         "dialect.rkt"
         "operator.rkt"
         "standard.rkt")

(provide
 sqlite3-dialect?
 sqlite3-dialect)

(define-values (sqlite3-dialect? sqlite3-dialect)
  (let ()
    (struct sqlite3-dialect ()
      #:methods gen:dialect
      [(define (dialect-name _) 'sqlite3)
       (define (dialect-supports-returning? _) #t)

       (define/contract (dialect-last-id-query _)
         (-> dialect? string?)
         "SELECT last_insert_rowid()")

       (define/contract (dialect-emit-ddl _ d)
         (-> dialect? ddl? string?)
         (emit-ddl d))

       (define/contract (dialect-emit-query/impl _ s)
         (-> dialect? stmt? string?)
         (emit-stmt s))

       (define/contract (dialect-prepare-parameters _ _p args)
         (-> dialect? prepared-statement? (listof any/c) (listof any/c))
         (for/list ([arg (in-list args)])
           (match arg
             [#f 0]
             [#t 1]
             [_ arg])))])

    (values sqlite3-dialect? (sqlite3-dialect))))

(define (emit-ddl d)
  (with-output-to-string
    (lambda ()
      (match d
        [(create-table table fields)
         (display "CREATE TABLE IF NOT EXISTS ")
         (display (quote/standard table))

         (display "(")
         (define n-fields (length fields))
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
  (display (quote/standard (field-name f)))
  (display " ")
  (display (type-declaration (field-type f) 'sqlite3))

  (unless (field-nullable? f)
    (display " NOT NULL"))

  (when (field-primary-key? f)
    (display " PRIMARY KEY"))

  (when (field-auto-increment? f)
    (display " AUTOINCREMENT"))

  (when (field-unique? f)
    (display " UNIQUE")))

(define (emit-field-constraints f)
  (when (field-foreign-key? f)
    (display ",")
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
  (match e
    [(app (ident (unary-operator 'json)) args)
     (write-string "JSON(")
     (emit-expr (car args))
     (write-string ")")]

    [(app (and (ident (or 'date 'time 'datetime)) op) args)
     (emit-expr op)
     (write-string "(")
     (write/sep args emit-expr)
     (write-string ")")]

    [_
     (emit-expr/standard e)]))

(define (emit-stmt e)
  (with-output-to-string
    (lambda ()
      (emit-stmt/sqlite3 e))))

(define (emit-stmt/sqlite3 e)
  (match e
    [(union stmt)
     (write-string "UNION ")
     (emit-stmt/sqlite3 stmt)]

    [_
     (emit-stmt/standard e)]))

(define emit-expr/standard
  (make-expr-emitter
   emit-expr
   emit-stmt/sqlite3))

(define emit-stmt/standard
  (make-stmt-emitter
   #:supports-returning? #t
   emit-stmt/sqlite3 emit-expr))
