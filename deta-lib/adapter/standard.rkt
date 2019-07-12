#lang racket/base

(require racket/format
         racket/match
         racket/port
         racket/string
         "ast.rkt")

(provide
 quote/standard
 emit-query/standard)

(define (quote/standard e)
  (~a #\" e #\"))

(define (emit-query/standard e
                             #:supports-returning? [supports-returning? #f])
  (match e
    [(? string?)
     (quote/standard e)]

    [(list es ...)
     (string-join (map emit-query/standard es) ", ")]

    [(qualified-name parent name)
     (~a (emit-query/standard parent) "." (emit-query/standard name))]

    [(alias-expr e alias)
     (~a (emit-query/standard e) " " (emit-query/standard alias))]

    [(column-expr e)
     (emit-query/standard e)]

    [(table-expr e)
     (emit-query/standard e)]

    [(select-stmt from columns where)
     (with-output-to-string
       (lambda _
         (display (~a "SELECT " (emit-query/standard columns) " " (emit-query/standard from)))
         (when where
           (display (emit-query/standard where)))))]

    [(insert-stmt table columns returning)
     (with-output-to-string
       (lambda _
         (define placeholders
           (for/list ([i (in-range 1 (add1 (length columns)))])
             (~a "$" i)))

         (display (~a "INSERT INTO " (emit-query/standard table)
                      " (" (emit-query/standard columns) ") "
                      " VALUES (" (string-join placeholders ", ") ")"))

         (when (and returning supports-returning?)
           (display (~a " RETURNING " (emit-query/standard returning))))))]

    [(from-clause _ table)
     (~a "FROM " (emit-query/standard table))]

    [(where-clause e)
     (~a "WHERE " (emit-query/standard e))]))
