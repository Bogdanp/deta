#lang at-exp racket/base

(require db
         db/util/postgresql
         gregor
         gregor/time
         json
         racket/contract
         racket/format
         racket/match
         "private/type.rkt")

(provide
 type?

 id/f?
 id/f

 integer/f?
 integer/f

 real/f?
 real/f

 numeric/f?
 numeric/f

 string/f?
 string/f

 binary/f?
 binary/f

 symbol/f?
 symbol/f

 boolean/f?
 boolean/f

 date/f?
 date/f

 time/f?
 time/f

 datetime/f?
 datetime/f

 datetime-tz/f?
 datetime-tz/f

 array/f?
 array/f

 json/f?
 json/f

 jsonb/f?
 jsonb/f)

(define gen:type-contract type-contract)
(define gen:type-declaration type-declaration)
(define gen:type-load type-load)
(define gen:type-dump type-dump)

(define-values (id/f? id/f)
  (let ()
    (struct id-field ()
      #:methods gen:type
      [(define (type-contract _) exact-nonnegative-integer?)
       (define (type-declaration _ dialect) "INTEGER")
       (define (type-load _ dialect v) v)
       (define (type-dump _ dialect v) v)])

    (values id-field? (id-field))))

(define-values (integer/f? integer/f)
  (let ()
    (struct integer-field ()
      #:methods gen:type
      [(define (type-contract _) exact-integer?)
       (define (type-declaration _ dialect) "INTEGER")
       (define (type-load _ dialect v) v)
       (define (type-dump _ dialect v) v)])

    (values integer-field? (integer-field))))

(define-values (real/f? real/f)
  (let ()
    (struct real-field ()
      #:methods gen:type
      [(define (type-contract _) real?)
       (define (type-declaration _ dialect) "REAL")
       (define (type-load _ dialect v) v)
       (define (type-dump _ dialect v) v)])

    (values real-field? (real-field))))

(define-values (numeric/f? numeric/f)
  (let ()
    (struct numeric-field (precision scale)
      #:methods gen:type
      [(define (type-contract _) (or/c rational? +nan.0))

       (define (type-declaration t dialect)
         @~a{NUMERIC(@(numeric-field-precision t), @(numeric-field-scale t))})

       (define (type-load _ dialect v) v)
       (define (type-dump _ dialect v) v)])

    (define/contract (make-numeric-field precision scale)
      (-> exact-positive-integer? exact-nonnegative-integer? numeric-field?)
      (numeric-field precision scale))

    (values numeric-field? numeric-field)))

(define-values (string/f? string/f)
  (let ()
    (struct string-field ()
      #:methods gen:type
      [(define (type-contract _) string?)
       (define (type-declaration _ dialect) "TEXT")
       (define (type-load _ dialect v) v)
       (define (type-dump _ dialect v) v)])

    (values string-field? (string-field))))

(define-values (binary/f? binary/f)
  (let ()
    (struct binary-field ()
      #:methods gen:type
      [(define (type-contract _) bytes?)
       (define (type-declaration _ dialect) "BLOB")
       (define (type-load _ dialect v) v)
       (define (type-dump _ dialect v) v)])

    (values binary-field? (binary-field))))

(define-values (symbol/f? symbol/f)
  (let ()
    (struct symbol-field ()
      #:methods gen:type
      [(define (type-contract _) symbol?)
       (define (type-declaration _ dialect) "TEXT")

       (define (type-load _ dialect v) (string->symbol v))
       (define (type-dump _ dialect v) (symbol->string v))])

    (values symbol-field? (symbol-field))))

(define-values (boolean/f? boolean/f)
  (let ()
    (struct boolean-field ()
      #:methods gen:type
      [(define (type-contract _) boolean?)

       (define (type-declaration _ dialect)
         (match dialect
           ['sqlite3    "INTEGER"]
           ['postgresql "BOOLEAN"]))

       (define (type-load _ dialect v)
         (match dialect
           ['sqlite3    (= v 1)]
           ['postgresql    v   ]))

       (define (type-dump _ dialect v)
         (match dialect
           ['sqlite3    (if v 1 0)]
           ['postgresql     v     ]))])

    (values boolean-field? (boolean-field))))

(define-values (date/f? date/f)
  (let ()
    (struct date-field ()
      #:methods gen:type
      [(define (type-contract _) date-provider?)

       (define (type-declaration _ dialect)
         (match dialect
           ['sqlite3    "TEXT"]
           ['postgresql "DATE"]))

       (define (type-load _ dialect v)
         (match dialect
           ['sqlite3
            (iso8601->date v)]

           ['postgresql
            (date (sql-date-year  v)
                  (sql-date-month v)
                  (sql-date-day   v))]))

       (define (type-dump _ dialect v)
         (match dialect
           ['sqlite3
            (date->iso8601 (->date v))]

           ['postgresql
            (sql-date (->year  v)
                      (->month v)
                      (->day   v))]))])

    (values date-field? (date-field))))

(define-values (time/f? time/f)
  (let ()
    (struct time-field ()
      #:methods gen:type
      [(define (type-contract _) time-provider?)

       (define (type-declaration _ dialect)
         (match dialect
           ['sqlite3    "TEXT"]
           ['postgresql "TIME"]))

       (define (type-load _ dialect v)
         (match dialect
           ['sqlite3
            (iso8601->time v)]

           ['postgresql
            (time (sql-time-hour       v)
                  (sql-time-minute     v)
                  (sql-time-second     v)
                  (sql-time-nanosecond v))]))

       (define (type-dump _ dialect v)
         (match dialect
           ['sqlite3
            (time->iso8601 (->time v))]

           ['postgresql
            (sql-time (->hours       v)
                      (->minutes     v)
                      (->seconds     v)
                      (->nanoseconds v))]))])

    (values time-field? (time-field))))

(define-values (datetime/f? datetime/f)
  (let ()
    (struct datetime-field ()
      #:methods gen:type
      [(define (type-contract _) datetime-provider?)

       (define (type-declaration _ dialect)
         (match dialect
           ['sqlite3    "TEXT"]
           ['postgresql "TIMESTAMP"]))

       (define (type-load _ dialect v)
         (match dialect
           ['sqlite3
            (iso8601->datetime v)]

           ['postgresql
            (datetime (sql-timestamp-year       v)
                      (sql-timestamp-month      v)
                      (sql-timestamp-day        v)
                      (sql-timestamp-hour       v)
                      (sql-timestamp-minute     v)
                      (sql-timestamp-second     v)
                      (sql-timestamp-nanosecond v))]))

       (define (type-dump _ dialect v)
         (match dialect
           ['sqlite3
            (datetime->iso8601 (->datetime/local v))]

           ['postgresql
            (sql-timestamp (->year        v)
                           (->month       v)
                           (->day         v)
                           (->hours       v)
                           (->minutes     v)
                           (->seconds     v)
                           (->nanoseconds v)
                           #f)]))])

    (values datetime-field? (datetime-field))))

(define-values (datetime-tz/f? datetime-tz/f)
  (let ()
    (struct datetime-tz-field ()
      #:methods gen:type
      [(define (type-contract _) moment-provider?)

       (define (type-declaration _ dialect)
         (match dialect
           ['sqlite3    "TEXT"]
           ['postgresql "TIMESTAMPTZ"]))

       (define (type-load _ dialect v)
         (match dialect
           ['sqlite3
            (iso8601/tzid->moment v)]

           ['postgresql
            (moment (sql-timestamp-year       v)
                    (sql-timestamp-month      v)
                    (sql-timestamp-day        v)
                    (sql-timestamp-hour       v)
                    (sql-timestamp-minute     v)
                    (sql-timestamp-second     v)
                    (sql-timestamp-nanosecond v)
                    #:tz (sql-timestamp-tz    v))]))

       (define (type-dump _ dialect v)
         (match dialect
           ['sqlite3
            (moment->iso8601/tzid (->moment v))]

           ['postgresql
            (sql-timestamp (->year        v)
                           (->month       v)
                           (->day         v)
                           (->hours       v)
                           (->minutes     v)
                           (->seconds     v)
                           (->nanoseconds v)
                           (->utc-offset  v))]))])

    (values datetime-tz-field? (datetime-tz-field))))

(define-values (array/f? array/f)
  (let ()
    (struct array-field (subtype size)
      #:methods gen:type
      [(define (type-contract t)
         (vectorof (gen:type-contract (array-field-subtype t))))

       (define (type-declaration t dialect)
         (match dialect
           ['postgresql
            (define subtype-declaration
              (gen:type-declaration (array-field-subtype t) dialect))

            (define size:str
              (cond
                [(array-field-size t) => number->string]
                [else ""]))

            (~a subtype-declaration  "[" size:str "]")]

           [_ (raise-user-error 'array/f "not supported")]))

       (define (type-load t dialect v)
         (define subtype (array-field-subtype t))
         (for/vector ([x (in-vector (pg-array-contents v))])
           (gen:type-load subtype dialect x)))

       (define (type-dump t dialect v)
         (define subtype (array-field-subtype t))
         (for/list ([x (in-vector v)])
           (gen:type-dump subtype dialect x)))])

    (values array-field?
            (lambda (subtype [size #f])
              (array-field subtype size)))))

(define-values (json/f? json/f)
  (let ()
    (struct json-field ()
      #:methods gen:type
      [(define (type-contract _) jsexpr?)

       (define (type-declaration t dialect)
         (match dialect
           ['postgresql "JSON"]
           [_ (raise-user-error 'json/f "not supported")]))

       (define (type-load _ dialect v) v)
       (define (type-dump _ dialect v) v)])

    (values json-field? (json-field))))

(define-values (jsonb/f? jsonb/f)
  (let ()
    (struct jsonb-field ()
      #:methods gen:type
      [(define (type-contract _) jsexpr?)

       (define (type-declaration t dialect)
         (match dialect
           ['postgresql "JSONB"]
           [_ (raise-user-error 'json/f "not supported")]))

       (define (type-load _ dialect v) v)
       (define (type-dump _ dialect v) v)])

    (values jsonb-field? (jsonb-field))))
