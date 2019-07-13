#lang at-exp racket/base

(require racket/format
         racket/match
         racket/port
         racket/string
         "../private/ast.rkt"
         "adapter.rkt")

(provide
 quote/standard
 make-expr-emitter
 make-stmt-emitter)

(define (quote/standard e)
  (~a #\" e #\"))

(define ((make-expr-emitter recur) e)
  (define (maybe-parenthize e)
    (if (expr-terminal? e)
        (recur e)
        (~a "(" (recur e) ")")))

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

    [(scalar v)
     (~v v)]

    [(qualified parent name)
     (~a (recur parent) "." (quote/standard name))]

    [(as e alias)
     (~a (maybe-parenthize e) " AS " (quote/standard alias))]

    [(app (and (name (or 'not 'interval)) op) (list a))
     (~a (recur op) " " (maybe-parenthize a))]

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
     (~a (maybe-parenthize a) " " (recur op) " " (maybe-parenthize b))]

    [(app (name 'between) (list a b c))
     (~a (maybe-parenthize a) " BETWEEN " (maybe-parenthize b) " AND " (maybe-parenthize c))]

    [(app f args)
     (~a (recur f) "(" (string-join (map recur args) ", ") ")")]

    [(column e)
     (recur e)]

    [(placeholder v)
     (~a "$" (track-placeholder! v))]

    [(table e)
     (recur e)]))

(define ((make-stmt-emitter recur emit-expr
                            #:supports-returning? [supports-returning? #f]) e)
  (match e
    [(list exprs ...)
     (string-join (map emit-expr exprs) ", ")]

    [(select columns from where group-by order-by offset)
     (with-output-to-string
       (lambda _
         (display @~a{SELECT @(recur columns)})
         (when from     (display (~a " " (recur from))))
         (when where    (display (~a " " (recur where))))
         (when group-by (display (~a " " (recur group-by))))
         (when order-by (display (~a " " (recur order-by))))
         (when offset   (display (~a " " (recur offset))))))]

    [(update table assignments where)
     (with-output-to-string
       (lambda _
         (display @~a{UPDATE @(emit-expr table)})
         (when assignments (display (~a " " (recur assignments))))
         (when where       (display (~a " " (recur where))))))]

    [(delete from where)
     @~a{DELETE @(recur from) @(recur where)}]

    [(insert table columns column-values returning)
     (with-output-to-string
       (lambda _
         (display @~a{INSERT INTO @(emit-expr table) (@(recur columns))})
         (display @~a{ VALUES (@(recur column-values))})
         (when (and returning supports-returning?)
           (display @~a{ RETURNING @(emit-expr returning)}))))]

    [(assignments pairs)
     (define pair:strs
       (for/list ([pair (in-list pairs)])
         @~a{@(emit-expr (car pair)) = @(emit-expr (cdr pair))}))

     @~a{SET @(string-join pair:strs ", ")}]

    [(from  t)        @~a{FROM @(emit-expr t)}]
    [(where e)        @~a{WHERE @(emit-expr e)}]
    [(group-by cols)  @~a{GROUP BY @(recur cols)}]
    [(offset n)       @~a{OFFSET @n}]

    [(order-by pairs)
     (define pair:strs
       (for/list ([pair (in-list pairs)])
         (define col-e (car pair))
         (define dir-e (cdr pair))
         (if (eq? dir-e 'desc)
             (~a (emit-expr col-e) " DESC")
             (emit-expr col-e))))

     (~a "ORDER BY " (string-join pair:strs ", "))]))
