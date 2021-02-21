#lang at-exp racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         db
         db/util/postgresql
         gregor
         gregor/time
         json
         racket/contract
         racket/format
         "private/type.rkt")

(provide
 type?)

(define (raise-dialect-error who d)
  (raise-user-error who "unsupported dialect ~s" d))

(define gen:type-contract type-contract)
(define gen:type-declaration type-declaration)
(define gen:type-load type-load)
(define gen:type-dump type-dump)

(define-syntax (define-type stx)
  (syntax-parse stx
    [(_ id:id (~optional (fld:id ...))
        (~alt
         (~optional (~or (~seq #:contract contract-e)
                         (~seq #:contract-fn contract-fn-e)))
         (~optional (~seq #:declaration declaration-e))
         (~optional (~seq #:load load-e))
         (~optional (~seq #:dump dump-e))
         (~optional (~seq #:constructor constructor-e))) ...)
     #:with id-field  (format-id #'id "~a-field" #'id)
     #:with id-field? (format-id #'id "~a-field?" #'id)
     #:with id/f      (format-id #'id "~a/f" #'id)
     #:with id/f?     (format-id #'id "~a/f?" #'id)

     #:fail-unless (attribute declaration-e)
     "every type must have a #:declaration"

     #'(begin
         (provide id/f id/f?)
         (struct id-field (~? (fld ...) ())
           #:methods gen:type
           [(define (type-contract type)
              (~? contract-e (~? (contract-fn-e type) any/c)))
            (define (type-declaration type dialect)
              (let ([decl declaration-e])
                (if (procedure? decl)
                    (decl type dialect)
                    decl)))
            (define (type-load type dialect v)
              (~? (load-e type dialect v) v))
            (define (type-dump type dialect v)
              (~? (dump-e type dialect v) v))])
         (define id/f? id-field?)
         (define id/f
           (~? constructor-e (id-field))))]))

(define-type id
  #:contract exact-nonnegative-integer?
  #:declaration "INTEGER")

(define-type integer
  #:contract exact-integer?
  #:declaration "INTEGER")

(define-type real
  #:contract real?
  #:declaration "REAL")

(define-type numeric (precision scale)
  #:contract (or/c rational? +nan.0)
  #:declaration
  (lambda (t _dialect)
    @~a{NUMERIC(@(numeric-field-precision t), @(numeric-field-scale t))})
  #:constructor
  (lambda (precision scale)
    (unless (exact-positive-integer? precision)
      (raise-argument-error 'numeric/f "exact-positive-integer?" precision))
    (unless (exact-nonnegative-integer? scale)
      (raise-argument-error 'numeric/f "exact-nonnegative-integer?" scale))
    (numeric-field precision scale)))

(define-type string
  #:contract string?
  #:declaration "TEXT")

(define-type binary
  #:contract bytes?
  #:declaration
  (lambda (_ dialect)
    (case dialect
      [(mysql sqlite3) "BLOB"]
      [(postgresql)    "BYTEA"]
      [else (raise-dialect-error 'binary/f dialect)])))

(define-type symbol
  #:contract symbol?
  #:declaration "TEXT"
  #:load
  (lambda (_ _dialect v)
    (string->symbol v))
  #:dump
  (lambda (_ _dialect v)
    (symbol->string v)))

(define-type boolean
  #:contract boolean?
  #:declaration
  (lambda (_ dialect)
    (case dialect
      [(sqlite3) "INTEGER"]
      [else      "BOOLEAN"]))
  #:load
  (lambda (_ dialect v)
    (case dialect
      [(sqlite3) (= v 1)]
      [else         v]))
  #:dump
  (lambda (_ dialect v)
    (case dialect
      [(sqlite3) (if v 1 0)]
      [else          v     ])))

(define-type date
  #:contract date-provider?
  #:declaration
  (lambda (_ dialect)
    (case dialect
      [(sqlite3) "TEXT"]
      [else      "DATE"]))
  #:load
  (lambda (_ dialect v)
    (case dialect
      [(sqlite3)
       (iso8601->date v)]

      [else
       (date (sql-date-year  v)
             (sql-date-month v)
             (sql-date-day   v))]))
  #:dump
  (lambda (_ dialect v)
    (case dialect
      [(sqlite3)
       (date->iso8601 (->date v))]

      [else
       (sql-date (->year  v)
                 (->month v)
                 (->day   v))])))

(define-type time
  #:contract time-provider?
  #:declaration
  (lambda (_ dialect)
    (case dialect
      [(sqlite3) "TEXT"]
      [else      "TIME"]))
  #:load
  (lambda (_ dialect v)
    (case dialect
      [(sqlite3)
       (iso8601->time v)]

      [else
       (time (sql-time-hour       v)
             (sql-time-minute     v)
             (sql-time-second     v)
             (sql-time-nanosecond v))]))
  #:dump
  (lambda (_ dialect v)
    (case dialect
      [(sqlite3)
       (time->iso8601 (->time v))]

      [else
       (sql-time (->hours       v)
                 (->minutes     v)
                 (->seconds     v)
                 (->nanoseconds v))])))

(define-type datetime
  #:contract datetime-provider?
  #:declaration
  (lambda (_ dialect)
    (case dialect
      [(mysql)      "DATETIME"]
      [(postgresql) "TIMESTAMP"]
      [(sqlite3)    "TEXT"]
      [else (raise-dialect-error 'datetime/f dialect)]))
  #:load
  (lambda (_ dialect v)
    (case dialect
      [(sqlite3)
       (iso8601->datetime v)]

      [else
       (datetime (sql-timestamp-year       v)
                 (sql-timestamp-month      v)
                 (sql-timestamp-day        v)
                 (sql-timestamp-hour       v)
                 (sql-timestamp-minute     v)
                 (sql-timestamp-second     v)
                 (sql-timestamp-nanosecond v))]))
  #:dump
  (lambda (_ dialect v)
    (case dialect
      [(sqlite3)
       (datetime->iso8601 (->datetime/local v))]

      [else
       (sql-timestamp (->year        v)
                      (->month       v)
                      (->day         v)
                      (->hours       v)
                      (->minutes     v)
                      (->seconds     v)
                      (->nanoseconds v)
                      #f)])))

(define-type datetime-tz
  #:contract moment-provider?
  #:declaration
  (lambda (_ dialect)
    (case dialect
      [(mysql)      "TIMESTAMP"]
      [(sqlite3)    "TEXT"]
      [(postgresql) "TIMESTAMPTZ"]))
  #:load
  (lambda (_ dialect v)
    (case dialect
      [(sqlite3)
       (iso8601/tzid->moment v)]

      [else
       (moment (sql-timestamp-year       v)
               (sql-timestamp-month      v)
               (sql-timestamp-day        v)
               (sql-timestamp-hour       v)
               (sql-timestamp-minute     v)
               (sql-timestamp-second     v)
               (sql-timestamp-nanosecond v)
               #:tz (sql-timestamp-tz    v))]))
  #:dump
  (lambda (_ dialect v)
    (case dialect
      [(sqlite3)
       (moment->iso8601/tzid (->moment v))]

      [else
       (sql-timestamp (->year        v)
                      (->month       v)
                      (->day         v)
                      (->hours       v)
                      (->minutes     v)
                      (->seconds     v)
                      (->nanoseconds v)
                      (->utc-offset  v))])))

(define-type array (subtype size)
  #:contract-fn
  (lambda (t)
    (vectorof (gen:type-contract (array-field-subtype t))))
  #:constructor
  (lambda (subtype [size #f])
    (array-field subtype size))
  #:declaration
  (lambda (t dialect)
    (case dialect
      [(postgresql)
       (define subtype-declaration
         (gen:type-declaration (array-field-subtype t) dialect))

       (define size:str
         (cond
           [(array-field-size t) => number->string]
           [else ""]))

       (~a subtype-declaration  "[" size:str "]")]

      [else
       (raise-dialect-error 'array/f dialect)]))
  #:load
  (lambda (t dialect v)
    (define subtype (array-field-subtype t))
    (for/vector ([x (in-vector (pg-array-contents v))])
      (gen:type-load subtype dialect x)))
  #:dump
  (lambda (t dialect v)
    (define subtype (array-field-subtype t))
    (for/list ([x (in-vector v)])
      (gen:type-dump subtype dialect x))))

(define-type json
  #:contract jsexpr?
  #:declaration
  (lambda (_ dialect)
    (case dialect
      [(mysql postgresql) "JSON"]
      [else (raise-dialect-error 'json/f dialect)])))

(define-type jsonb
  #:contract jsexpr?
  #:declaration
  (lambda (_ dialect)
    (case dialect
      [(postgresql) "JSONB"]
      [else (raise-dialect-error 'jsonb/f dialect)])))

(define-type uuid
  #:contract uuid?
  #:declaration
  (lambda (_ dialect)
    (case dialect
      [(postgresql) "UUID"]
      [else (raise-dialect-error 'uuid/f dialect)])))

(define-type any
  #:contract any/c
  #:declaration
  (lambda (_ _dialect)
    (raise-user-error 'any/f "may only be used with virtual fields"))
  #:dump
  (lambda (_ _dialect _v)
    (raise-user-error 'any/f "may not be stored in the database")))
