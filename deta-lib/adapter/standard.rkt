#lang at-exp racket/base

(require racket/format
         racket/match
         racket/port
         racket/string
         "../private/ast.rkt")

(provide
 quote/standard
 make-expr-emitter
 make-stmt-emitter)

(define (quote/standard e)
  (~a #\" e #\"))

(define ((make-expr-emitter recur) e)
  (match e
    [(? string?)
     (quote/standard e)]

    [(name n)
     (symbol->string n)]

    [(scalar #t) "true"]
    [(scalar #f) "false"]

    [(scalar (and (? string?) v))
     (~a "'" (string-replace v "'" "''") "'")]

    [(scalar v)
     (~v v)]

    [(qualified parent name)
     (~a (recur parent) "." (quote/standard name))]

    [(as e alias)
     (~a (recur e) " AS " (quote/standard alias))]

    [(app (name (and (or 'and 'or 'like '= '> '< '>= '<=) op)) (list a b))
     (~a (recur a) " " (string-upcase (symbol->string op)) " " (recur b))]

    [(app f args)
     (~a (recur f) "(" (string-join (map recur args) ", ") ")")]

    [(column e)
     (recur e)]

    [(placeholder n)
     (~a "$" n)]

    [(table e)
     (recur e)]))

(define ((make-stmt-emitter recur emit-expr
                            #:supports-returning? [supports-returning? #f]) e)
  (match e
    [(list exprs ...)
     (string-join (map emit-expr exprs) ", ")]

    [(select from columns where)
     (with-output-to-string
       (lambda _
         (display @~a{SELECT @(recur columns) @(recur from)})
         (when where
           (display (~a " " (recur where))))))]

    [(delete from where)
     @~a{DELETE @(recur from) @(recur where)}]

    [(insert table columns column-values returning)
     (with-output-to-string
       (lambda _
         (display @~a{INSERT INTO @(emit-expr table) (@(recur columns))})
         (display @~a{ VALUES (@(recur column-values))})
         (when (and returning supports-returning?)
           (display @~a{ RETURNING @(emit-expr returning)}))))]

    [(from  t) @~a{FROM @(emit-expr t)}]
    [(where e) @~a{WHERE @(emit-expr e)}]))
