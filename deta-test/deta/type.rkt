#lang racket/base

(require db
         deta
         deta/private/field
         deta/private/type
         gregor
         racket/match
         rackunit)

(define-schema kitchen-sink
  #:virtual
  ([dates-vec (array/f date/f) #:nullable]))

(define (field-ref id)
  (for/first ([f (schema-fields kitchen-sink-schema)]
              #:when (eq? (field-id f) id))
    f))

(define type-tests
  (test-suite
   "type"

   (test-suite
    "array"

    (test-case "can dump and load dumped values for postgresql"
      (define initial (vector (date 1996 5 29)
                              (date 2019 5 29)))

      (define f (field-ref 'dates-vec))
      (define dumped
        (let ()
          (match-define (list (cons _ getter))
            (type-dump (field-type f) f 'postgresql))
          (getter (make-kitchen-sink #:dates-vec initial))))

      (check-equal? dumped (vector (sql-date 1996 5 29)
                                   (sql-date 2019 5 29)))

      (define loaded
        (let ()
          (match-define (list (cons _ value))
            (type-load (field-type f) f dumped 'postgresql))
          value))

      (check-equal? loaded initial))

    (test-case "can dump and load dumped values for sqlite3"
      (define initial (vector (date 1996 5 29)
                              (date 2019 5 29)))

      (define f (field-ref 'dates-vec))
      (define dumped
        (let ()
          (match-define (list (cons _ getter))
            (type-dump (field-type f) f 'sqlite3))
          (getter (make-kitchen-sink #:dates-vec initial))))

      (check-equal? dumped (vector "1996-05-29"
                                   "2019-05-29"))

      (define loaded
        (let ()
          (match-define (list (cons _ value))
            (type-load (field-type f) f dumped 'sqlite3))
          value))

      (check-equal? loaded initial)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests type-tests))
