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

    (test-case "entites with the same schema and data not equal"
      (define d (now/moment))
      (check-equal?
       (make-book #:title "A Title" #:author "An Author" #:published-at d)
       (make-book #:title "A Title" #:author "An Author" #:published-at d))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests entity-tests))
