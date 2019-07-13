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

    [(name 'bitwise-not)     "~"]
    [(name 'bitwise-and)     "&"]
    [(name 'bitwise-or )     "|"]
    [(name 'bitwise-xor)     "#"]
    [(name 'concat)          "||"]
    [(name 'is-distinct)     "IS DISTINCT"]
    [(name 'is-not-distinct) "IS NOT DISTINCT"]
    [(name 'is-not)          "IS NOT"]
    [(name 'not-like)        "NOT LIKE"]

    [(name n)
     (string-upcase (symbol->string n))]

    [(scalar #t) "TRUE"]
    [(scalar #f) "FALSE"]

    [(scalar (and (? string?) v))
     (~a "'" (string-replace v "'" "''") "'")]

    [(scalar (and (? exact-integer?) v))
     (~v v)]

    [(qualified parent name)
     (~a (recur parent) "." (quote/standard name))]

    [(as e alias)
     (~a (recur e) " AS " (quote/standard alias))]

    [(app (and (name (or 'not 'interval)) op) (list a))
     (~a (recur op) " " (recur a))]

    [(app (and (name (or
                      ;; bitwise ops: https://www.postgresql.org/docs/current/functions-math.html
                      'bitwise-not 'bitwise-and 'bitwise-or 'bitwise-xor '<< '>>

                      ;; logical ops: https://www.postgresql.org/docs/current/functions-logical.html
                      'and 'or

                      ;; comparison ops: https://www.postgresql.org/docs/current/functions-comparison.html
                      '= '> '< '>= '<= '<> '!= 'like 'not-like 'is 'is-not 'is-distinct 'is-not-distinct

                      ;; math ops: https://www.postgresql.org/docs/current/functions-math.html
                      '+ '- '* '/ '% '<< '>>

                      ;; string ops: https://www.postgresql.org/docs/current/functions-string.html
                      'concat
                      ))
               op)
          (list a b))
     (~a (recur a) " " (recur op) " " (recur b))]

    [(app (name 'between) (list a b c))
     (~a (recur a) " BETWEEN " (recur b) " AND " (recur c))]

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

    [(select columns from where)
     (with-output-to-string
       (lambda _
         (display @~a{SELECT @(recur columns)})
         (when from  (display (~a " " (recur from))))
         (when where (display (~a " " (recur where))))))]

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
