#lang at-exp racket/base

(require db
         gregor
         gregor/time
         json
         racket/contract
         racket/format
         racket/function
         racket/generic
         racket/match
         "private/field.rkt"
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
      [(define (type-contract _)
         exact-nonnegative-integer?)

       (define (type-declaration _ dialect)
         "INTEGER")

       (define (type-load _ f v dialect)
         (values (field-kwd f) v))

       (define (type-dump _ f dialect)
         (values (field-name f)
                 (field-getter f)))])

    (values id-field? (id-field))))

(define-values (integer/f? integer/f)
  (let ()
    (struct integer-field ()
      #:methods gen:type
      [(define (type-contract _)
         exact-integer?)

       (define (type-declaration _ dialect)
         "INTEGER")

       (define (type-load _ f v dialect)
         (values (field-kwd f) v))

       (define (type-dump _ f dialect)
         (values (field-name f)
                 (field-getter f)))])

    (values integer-field? (integer-field))))

(define-values (real/f? real/f)
  (let ()
    (struct real-field ()
      #:methods gen:type
      [(define (type-contract _)
         real?)

       (define (type-declaration _ dialect)
         "REAL")

       (define (type-load _ f v dialect)
         (values (field-kwd f) v))

       (define (type-dump _ f dialect)
         (values (field-name f)
                 (field-getter f)))])

    (values real-field? (real-field))))

(define-values (numeric/f? numeric/f)
  (let ()
    (struct numeric-field (precision scale)
      #:methods gen:type
      [(define (type-contract _)
         (or/c rational? +nan.0))

       (define (type-declaration t dialect)
         @~a{NUMERIC(@(numeric-field-precision t), @(numeric-field-scale t))})

       (define (type-load _ f v dialect)
         (values (field-kwd f) v))

       (define (type-dump _ f dialect)
         (values (field-name f)
                 (field-getter f)))])

    (define/contract (make-numeric-field precision scale)
      (-> exact-positive-integer? exact-nonnegative-integer? numeric-field?)
      (numeric-field precision scale))

    (values numeric-field? numeric-field)))

(define-values (string/f? string/f)
  (let ()
    (struct string-field ()
      #:methods gen:type
      [(define (type-contract _)
         string?)

       (define (type-declaration _ dialect)
         "TEXT")

       (define (type-load _ f v dialect)
         (values (field-kwd f) v))

       (define (type-dump _ f dialect)
         (values (field-name f)
                 (field-getter f)))])

    (values string-field? (string-field))))

(define-values (binary/f? binary/f)
  (let ()
    (struct binary-field ()
      #:methods gen:type
      [(define (type-contract _)
         bytes?)

       (define (type-declaration _ dialect)
         "BLOB")

       (define (type-load _ f v dialect)
         (values (field-kwd f) v))

       (define (type-dump _ f dialect)
         (values (field-name f)
                 (field-getter f)))])

    (values binary-field? (binary-field))))

(define-values (symbol/f? symbol/f)
  (let ()
    (struct symbol-field ()
      #:methods gen:type
      [(define (type-contract _)
         symbol?)

       (define (type-declaration _ dialect)
         "TEXT")

       (define (type-load _ f v dialect)
         (values (field-kwd f)
                 (cond
                   [(string? v) => string->symbol]
                   [else v])))

       (define (type-dump _ f dialect)
         (values (field-name f)
                 (compose1 (lambda (v)
                             (cond
                               [(symbol? v) => symbol->string]
                               [else v]))
                           (field-getter f))))])

    (values symbol-field? (symbol-field))))

(define-values (boolean/f? boolean/f)
  (let ()
    (struct boolean-field ()
      #:methods gen:type
      [(define (type-contract _)
         boolean?)

       (define (type-declaration _ dialect)
         (match dialect
           ['sqlite3    "INTEGER"]
           ['postgresql "BOOLEAN"]))

       (define (type-load _ f v dialect)
         (values (field-kwd f)
                 (match dialect
                   ['sqlite3    (= v 1)]
                   ['postgresql    v   ])))

       (define (type-dump _ f dialect)
         (values (field-name f)
                 (match dialect
                   ['sqlite3
                    (compose1 (match-lambda
                                [#f 0]
                                [#t 1])
                              (field-getter f))]

                   ['postgresql
                    (field-getter f)])))])

    (values boolean-field? (boolean-field))))

(define-values (date/f? date/f)
  (let ()
    (struct date-field ()
      #:methods gen:type
      [(define (type-contract _)
         date-provider?)

       (define (type-declaration _ dialect)
         (match dialect
           ['sqlite3    "TEXT"]
           ['postgresql "DATE"]))

       (define (type-load _ f v dialect)
         (values (field-kwd f)
                 (match dialect
                   ['sqlite3
                    (iso8601->date v)]

                   ['postgresql
                    (date (sql-date-year  v)
                          (sql-date-month v)
                          (sql-date-day   v))])))

       (define (type-dump _ f dialect)
         (values (field-name f)
                 (match dialect
                   ['sqlite3
                    (compose1 date->iso8601 ->date (field-getter f))]

                   ['postgresql
                    (compose1 (lambda (d)
                                (sql-date (->year  d)
                                          (->month d)
                                          (->day   d)))
                              (field-getter f))])))])

    (values date-field? (date-field))))

(define-values (time/f? time/f)
  (let ()
    (struct time-field ()
      #:methods gen:type
      [(define (type-contract _)
         time-provider?)

       (define (type-declaration _ dialect)
         (match dialect
           ['sqlite3    "TEXT"]
           ['postgresql "TIME"]))

       (define (type-load _ f v dialect)
         (values (field-kwd f)
                 (match dialect
                   ['sqlite3
                    (iso8601->time v)]

                   ['postgresql
                    (time (sql-time-hour       v)
                          (sql-time-minute     v)
                          (sql-time-second     v)
                          (sql-time-nanosecond v))])))

       (define (type-dump _ f dialect)
         (values (field-name f)
                 (match dialect
                   ['sqlite3
                    (compose1 time->iso8601 ->time (field-getter f))]

                   ['postgresql
                    (compose1 (lambda (t)
                                (sql-time (->hours       t)
                                          (->minutes     t)
                                          (->seconds     t)
                                          (->nanoseconds t)))
                              (field-getter f))])))])

    (values time-field? (time-field))))

(define-values (datetime/f? datetime/f)
  (let ()
    (struct datetime-field ()
      #:methods gen:type
      [(define (type-contract _)
         datetime-provider?)

       (define (type-declaration _ dialect)
         (match dialect
           ['sqlite3    "TEXT"]
           ['postgresql "TIMESTAMP"]))

       (define (type-load _ f v dialect)
         (values (field-kwd f)
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
                              (sql-timestamp-nanosecond v))])))

       (define (type-dump _ f dialect)
         (values (field-name f)
                 (match dialect
                   ['sqlite3
                    (compose1 datetime->iso8601 ->datetime/local (field-getter f))]

                   ['postgresql
                    (compose1 (lambda (dt)
                                (sql-timestamp (->year        dt)
                                               (->month       dt)
                                               (->day         dt)
                                               (->hours       dt)
                                               (->minutes     dt)
                                               (->seconds     dt)
                                               (->nanoseconds dt)))
                              (field-getter f))])))])

    (values datetime-field? (datetime-field))))

(define-values (datetime-tz/f? datetime-tz/f)
  (let ()
    (struct datetime-tz-field ()
      #:methods gen:type
      [(define (type-contract _)
         moment-provider?)

       (define (type-declaration _ dialect)
         (match dialect
           ['sqlite3    "TEXT"]
           ['postgresql "TIMESTAMPTZ"]))

       (define (type-load _ f v dialect)
         (values (field-kwd f)
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
                            #:tz (sql-timestamp-tz    v))])))

       (define (type-dump _ f dialect)
         (values (field-name f)
                 (match dialect
                   ['sqlite3
                    (compose1 moment->iso8601/tzid ->moment (field-getter f))]

                   ['postgresql
                    (compose1 (lambda (m)
                                (sql-timestamp (->year        m)
                                               (->month       m)
                                               (->day         m)
                                               (->hours       m)
                                               (->minutes     m)
                                               (->seconds     m)
                                               (->nanoseconds m)
                                               (->utc-offset  m)))
                              (field-getter f))])))])

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

       (define (type-load t f v dialect)
         (define subtype (array-field-subtype t))
         (define v:loaded
           (for/vector ([x (in-vector v)])
             (define-values (_ x*)
               (gen:type-load subtype f x dialect))

             x*))

         (values (field-kwd f) v:loaded))

       (define (type-dump t f dialect)
         (define subtype (array-field-subtype t))

         (values (field-kwd f)
                 (compose1 (lambda (v)
                             (for/vector ([x (in-vector v)])
                               (define f* (struct-copy field f [getter (const x)]))
                               (define-values (_ dumper)
                                 (gen:type-dump subtype f* dialect))

                               (dumper)))
                           (field-getter f))))])

    (values array-field?
            (lambda (subtype [size #f])
              (array-field subtype size)))))

(define-values (json/f? json/f)
  (let ()
    (struct json-field ()
      #:methods gen:type
      [(define (type-contract _)
         jsexpr?)

       (define (type-declaration t dialect)
         (match dialect
           ['postgresql "JSON"]
           [_ (raise-user-error 'json/f "not supported")]))

       (define (type-load _ f v dialect)
         (values (field-kwd f) v))

       (define (type-dump _ f dialect)
         (values (field-name f)
                 (field-getter f)))])

    (values json-field? (json-field))))

(define-values (jsonb/f? jsonb/f)
  (let ()
    (struct jsonb-field ()
      #:methods gen:type
      [(define (type-contract _)
         jsexpr?)

       (define (type-declaration t dialect)
         (match dialect
           ['postgresql "JSONB"]
           [_ (raise-user-error 'json/f "not supported")]))

       (define (type-load _ f v dialect)
         (values (field-kwd f) v))

       (define (type-dump _ f dialect)
         (values (field-name f)
                 (field-getter f)))])

    (values jsonb-field? (jsonb-field))))
