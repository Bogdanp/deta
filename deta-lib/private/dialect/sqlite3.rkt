#lang racket/base

(require racket/contract
         racket/match
         racket/port
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
  (with-output-to-string
    (lambda _
      (match d
        [(create-table table fields)
         (display "CREATE TABLE IF NOT EXISTS ")
         (display (quote/standard table))

         (displayln "(")
         (define n-fields (length fields))
         (for ([i (in-naturals 1)]
               [f (in-list fields)])
           (emit-field-ddl f (= i n-fields)))
         (displayln ")")]

        [(drop-table table)
         (display "DROP TABLE IF EXISTS ")
         (displayln (quote/standard table))]))))

(define (emit-field-ddl f last?)
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
    (display " UNIQUE"))

  (unless last?
    (displayln ",")))

(define (emit-expr e)
  (match e
    [_ (emit-expr/standard e)]))

(define emit-expr/standard
  (make-expr-emitter emit-expr
                     (lambda args
                       (apply emit-stmt/sqlite3 args))))

(define (emit-stmt e)
  (with-output-to-string
    (lambda _
      (emit-stmt/sqlite3 e))))

(define (emit-stmt/sqlite3 e)
  (emit-stmt/standard e))

(define emit-stmt/standard
  (make-stmt-emitter emit-stmt/sqlite3 emit-expr))
