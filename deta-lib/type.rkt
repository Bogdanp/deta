#lang racket/base

(require db
         gregor
         gregor/time
         json
         racket/contract
         racket/generic
         racket/match
         "field.rkt"
         "private/type.rkt")

(provide
 id/f?
 id/f

 integer/f?
 integer/f

 real/f?
 real/f

 numeric/f?
 numeric/f
 numeric/f-precision
 numeric/f-scale

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

 json/f?
 json/f

 jsonb/f?
 jsonb/f)

(define-values (id/f? id/f)
  (let ()
    (struct id-field ()
      #:methods gen:type
      [(define (type-contract _)
         exact-nonnegative-integer?)

       (define (type-load _ f v)
         (list (cons (field-kwd f) v)))

       (define (type-dump _ f)
         (list (cons (field-name f)
                     (field-getter f))))])

    (values id-field? (id-field))))

(define-values (integer/f? integer/f)
  (let ()
    (struct integer-field ()
      #:methods gen:type
      [(define (type-contract _)
         exact-integer?)

       (define (type-load _ f v)
         (list (cons (field-kwd f) v)))

       (define (type-dump _ f)
         (list (cons (field-name f)
                     (field-getter f))))])

    (values integer-field? (integer-field))))

(define-values (real/f? real/f)
  (let ()
    (struct real-field ()
      #:methods gen:type
      [(define (type-contract _)
         real?)

       (define (type-load _ f v)
         (list (cons (field-kwd f) v)))

       (define (type-dump _ f)
         (list (cons (field-name f)
                     (field-getter f))))])

    (values real-field? (real-field))))

(define-values (numeric/f? numeric/f numeric/f-precision numeric/f-scale)
  (let ()
    (struct numeric-field (precision scale)
      #:methods gen:type
      [(define (type-contract _)
         (or/c rational? +nan.0))

       (define (type-load _ f v)
         (list (cons (field-kwd f) v)))

       (define (type-dump _ f)
         (list (cons (field-name f)
                     (field-getter f))))])

    (values numeric-field?
            numeric-field
            numeric-field-precision
            numeric-field-scale)))

(define-values (string/f? string/f)
  (let ()
    (struct string-field ()
      #:methods gen:type
      [(define (type-contract _)
         string?)

       (define (type-load _ f v)
         (list (cons (field-kwd f) v)))

       (define (type-dump _ f)
         (list (cons (field-name f)
                     (field-getter f))))])

    (values string-field? (string-field))))

(define-values (binary/f? binary/f)
  (let ()
    (struct binary-field ()
      #:methods gen:type
      [(define (type-contract _)
         bytes?)

       (define (type-load _ f v)
         (list (cons (field-kwd f) v)))

       (define (type-dump _ f)
         (list (cons (field-name f)
                     (field-getter f))))])

    (values binary-field? (binary-field))))

(define-values (symbol/f? symbol/f)
  (let ()
    (struct symbol-field ()
      #:methods gen:type
      [(define (type-contract _)
         symbol?)

       (define (type-load _ f v)
         (list (cons (field-kwd f) (cond
                                     [(string? v) => string->symbol]
                                     [else v]))))

       (define (type-dump _ f)
         (list (cons (field-name f)
                     (compose1 (lambda (v)
                                 (cond
                                   [(symbol? v) => symbol->string]
                                   [else v]))
                               (field-getter f)))))])

    (values symbol-field? (symbol-field))))

(define-values (boolean/f? boolean/f)
  (let ()
    (struct boolean-field ()
      #:methods gen:type
      [(define (type-contract _)
         boolean?)

       (define (type-load _ f v)
         (list (cons (field-kwd f) v)))

       (define (type-dump _ f)
         (list (cons (field-name f)
                     (field-getter f))))])

    (values boolean-field? (boolean-field))))

(define-values (date/f? date/f)
  (let ()
    (struct date-field ()
      #:methods gen:type
      [(define (type-contract _)
         date-provider?)

       (define (type-load _ f v)
         (list (cons (field-kwd f)
                     (date (sql-date-year  v)
                           (sql-date-month v)
                           (sql-date-day   v)))))

       (define (type-dump _ f)
         (list (cons (field-name f)
                     (compose1 (lambda (d)
                                 (sql-date (->year  d)
                                           (->month d)
                                           (->day   d)))
                               (field-getter f)))))])

    (values date-field? (date-field))))

(define-values (time/f? time/f)
  (let ()
    (struct time-field ()
      #:methods gen:type
      [(define (type-contract _)
         time-provider?)

       (define (type-load _ f v)
         (list (cons (field-kwd f)
                     (time (sql-time-hour       v)
                           (sql-time-minute     v)
                           (sql-time-second     v)
                           (sql-time-nanosecond v)))))

       (define (type-dump _ f)
         (list (cons (field-name f)
                     (compose1 (lambda (t)
                                 (sql-time (->hours       t)
                                           (->minutes     t)
                                           (->seconds     t)
                                           (->nanoseconds t)))
                               (field-getter f)))))])

    (values time-field? (time-field))))

(define-values (datetime/f? datetime/f)
  (let ()
    (struct datetime-field ()
      #:methods gen:type
      [(define (type-contract _)
         datetime-provider?)

       (define (type-load _ f v)
         (list (cons (field-kwd f)
                     (datetime (sql-timestamp-year       v)
                               (sql-timestamp-month      v)
                               (sql-timestamp-day        v)
                               (sql-timestamp-hour       v)
                               (sql-timestamp-minute     v)
                               (sql-timestamp-second     v)
                               (sql-timestamp-nanosecond v)))))

       (define (type-dump _ f)
         (list (cons (field-name f)
                     (compose1 (lambda (dt)
                                 (sql-timestamp (->year        dt)
                                                (->month       dt)
                                                (->day         dt)
                                                (->hours       dt)
                                                (->minutes     dt)
                                                (->seconds     dt)
                                                (->nanoseconds dt)))
                               (field-getter f)))))])

    (values datetime-field? (datetime-field))))

(define-values (datetime-tz/f? datetime-tz/f)
  (let ()
    (struct datetime-tz-field ()
      #:methods gen:type
      [(define (type-contract _)
         moment-provider?)

       (define (type-load _ f v)
         (list (cons (field-kwd f)
                     (moment (sql-timestamp-year       v)
                             (sql-timestamp-month      v)
                             (sql-timestamp-day        v)
                             (sql-timestamp-hour       v)
                             (sql-timestamp-minute     v)
                             (sql-timestamp-second     v)
                             (sql-timestamp-nanosecond v)
                             #:tz (sql-timestamp-tz    v)))))

       (define (type-dump _ f)
         (list (cons (field-name f)
                     (compose1 (lambda (m)
                                 (sql-timestamp (->year        m)
                                                (->month       m)
                                                (->day         m)
                                                (->hours       m)
                                                (->minutes     m)
                                                (->seconds     m)
                                                (->nanoseconds m)
                                                (->utc-offset  m)))
                               (field-getter f)))))])

    (values datetime-tz-field? (datetime-tz-field))))

(define-values (json/f? json/f)
  (let ()
    (struct json-field ()
      #:methods gen:type
      [(define (type-contract _)
         jsexpr?)

       (define (type-load _ f v)
         (list (cons (field-kwd f) v)))

       (define (type-dump _ f)
         (list (cons (field-name f)
                     (field-getter f))))])

    (values json-field? (json-field))))

(define-values (jsonb/f? jsonb/f)
  (let ()
    (struct jsonb-field ()
      #:methods gen:type
      [(define (type-contract _)
         jsexpr?)

       (define (type-load _ f v)
         (list (cons (field-kwd f) v)))

       (define (type-dump _ f)
         (list (cons (field-name f)
                     (field-getter f))))])

    (values jsonb-field? (jsonb-field))))
