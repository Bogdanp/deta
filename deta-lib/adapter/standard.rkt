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
     (~a (recur parent) "." (quote/standard name))]

    [(alias-expr e alias)
     (~a (recur e) " " (quote/standard alias))]

    [(binary-expr op a b)
     (~a (recur a) " " op " " (recur b))]

    [(column-expr (and (? string?) name))
     (quote/standard name)]

    [(column-expr e)
     (recur e)]

    [(placeholder-expr n)
     (~a "$" n)]

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
         (when where
           (display (~a " " (recur where))))))]

    [(delete-stmt from where)
     @~a{DELETE @(recur from) @(recur where)}]

    [(insert-stmt table columns column-values returning)
     (with-output-to-string
       (lambda _
         (display @~a{INSERT INTO @(emit-expr table) (@(recur columns))})
         (display @~a{ VALUES (@(recur column-values))})
         (when (and returning supports-returning?)
           (display @~a{ RETURNING @(emit-expr returning)}))))]

    [(from-clause _ t) @~a{FROM  @(emit-expr t)}]
    [(where-clause  e) @~a{WHERE @(emit-expr e)}]))
