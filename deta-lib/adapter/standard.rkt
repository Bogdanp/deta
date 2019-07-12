#lang at-exp racket/base

(require racket/format
         racket/match
         racket/port
         racket/string
         "ast.rkt")

(provide
 quote/standard
 make-expr-emitter
 make-stmt-emitter)

(define (quote/standard e)
  (~a #\" e #\"))

(define ((make-expr-emitter recur) e)
  (match e
    [(qualified-name parent name)
     (~a (recur parent) "." (recur name))]

    [(alias-expr e alias)
     (~a (recur e) " " (quote/standard alias))]

    [(column-expr (and (? string?) name))
     (quote/standard name)]

    [(column-expr e)
     (recur e)]

    [(table-expr (and (? string?) name))
     (quote/standard name)]

    [(table-expr e)
     (recur e)]

    [_ (error 'foo)]))

(define ((make-stmt-emitter recur emit-expr
                            #:supports-returning? [supports-returning? #f]) e)
  (match e
    [(list exprs ...)
     (string-join (map emit-expr exprs) ", ")]

    [(select-stmt from columns where)
     (with-output-to-string
       (lambda _
         (display @~a{SELECT @(recur columns) @(recur from)})
         (when where (display (recur where)))))]

    [(insert-stmt table columns returning)
     (with-output-to-string
       (lambda _
         (define placeholders
           (for/list ([i (in-range 1 (add1 (length columns)))])
             (~a "$" i)))

         (display @~a{INSERT INTO @(emit-expr table) (@(recur columns))
                             VALUES (@(string-join placeholders ", "))})
         (when (and returning supports-returning?)
           (display @~a{ RETURNING @(emit-expr returning)}))))]

    [(from-clause _ t) @~a{ FROM  @(emit-expr t)}]
    [(where-clause  e) @~a{ WHERE @(emit-expr e)}]))
