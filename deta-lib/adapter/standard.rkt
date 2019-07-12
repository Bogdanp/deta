#lang racket/base

(require racket/format
         racket/match
         racket/port
         racket/string
         "ast.rkt")

(provide
 quote/standard
 emit/standard)

(define (quote/standard e)
  (~a #\" e #\"))

(define (emit/standard e
                       #:supports-returning? [supports-returning? #f])
  (match e
    [(? string?)
     (quote/standard e)]

    [(list es ...)
     (string-join (map emit/standard es) ", ")]

    [(qualified-name parent name)
     (~a (emit/standard parent) "." (emit/standard name))]

    [(alias-expr e alias)
     (~a (emit/standard e) " " (emit/standard alias))]

    [(column-expr e)
     (emit/standard e)]

    [(table-expr e)
     (emit/standard e)]

    [(select-stmt from columns where)
     (with-output-to-string
       (lambda _
         (display (~a "SELECT " (emit/standard columns) " " (emit/standard from)))
         (when where
           (display (emit/standard where)))))]

    [(insert-stmt table columns returning)
     (with-output-to-string
       (lambda _
         (define placeholders
           (for/list ([i (in-range 1 (add1 (length columns)))])
             (~a "$" i)))

         (display (~a "INSERT INTO " (emit/standard table)
                      " (" (emit/standard columns) ") "
                      " VALUES (" (string-join placeholders ", ") ")"))

         (when (and returning supports-returning?)
           (display (~a " RETURNING " (emit/standard returning))))))]

    [(from-clause _ table)
     (~a "FROM " (emit/standard table))]

    [(where-clause e)
     (~a "WHERE " (emit/standard e))]))
