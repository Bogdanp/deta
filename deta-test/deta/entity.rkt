#lang racket/base

(require deta
         gregor
         rackunit)

(provide
 entity-tests)

(define-schema book
  #:virtual
  ([title string/f]
   [author string/f]
   [published-at datetime-tz/f]))

(define-schema article
  #:virtual
  ([title string/f]
   [author string/f]
   [published-at datetime-tz/f]))

(define entity-tests
  (test-suite
   "entity"

   (test-suite
    "entity=?"

    (test-case "entites with different schemas are not equal"
      (define d (now/moment))
      (check-not-equal?
       (make-book    #:title "A Title" #:author "An Author" #:published-at d)
       (make-article #:title "A Title" #:author "An Author" #:published-at d)))

    (test-case "entites with different data are not equal"
      (define d (now/moment))
      (check-not-equal?
       (make-book #:title "A Title" #:author "An Author" #:published-at d)
       (make-book #:title "A Title" #:author "A Different Author" #:published-at d)))

    (test-case "entites with the same schema and data are equal"
      (define d (now/moment))
      (check-equal?
       (make-book #:title "A Title" #:author "An Author" #:published-at d)
       (make-book #:title "A Title" #:author "An Author" #:published-at d))))

   (test-suite
    "entity->hash"

    (test-case "converts entities to hashes"
      (define published-at
        (iso8601->moment "1954-07-29T00:00:00Z"))
      (define b
        (make-book
         #:title "Lord of the Rings"
         #:author "J. R. R. Tolkien"
         #:published-at (iso8601->moment "1954-07-29T00:00:00Z")))

      (check-equal?
       (entity->hash b)
       (hasheq
        'title "Lord of the Rings"
        'author "J. R. R. Tolkien"
        'published-at published-at))

      (check-equal?
       (entity->hash b (λ (_ v)
                         (cond
                           [(moment? v)
                            (moment->iso8601 v)]
                           [else
                            v])))
       (hasheq
        'title "Lord of the Rings"
        'author "J. R. R. Tolkien"
        'published-at "1954-07-29T00:00:00Z"))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests entity-tests))
