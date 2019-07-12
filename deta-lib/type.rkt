#lang racket/base

(require db
         racket/generic
         racket/match
         "private/field.rkt"
         "private/type.rkt")

(provide
 id/f?
 id/f

 integer/f?
 integer/f

 string/f?
 string/f

 symbol/f?
 symbol/f

 boolean/f?
 boolean/f)

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

    (values id-field?
            (id-field))))

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

    (values integer-field?
            (integer-field))))

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

    (values string-field?
            (string-field))))

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

    (values symbol-field?
            (symbol-field))))

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

    (values boolean-field?
            (boolean-field))))
