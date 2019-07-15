#lang at-exp racket/base

(require racket/format
         racket/match
         racket/port
         racket/string
         "../ast.rkt"
         "adapter.rkt")

;; Implements a "standard" emitter for SQL from our AST.  I say
;; "standard", but what I really mean is as close to the standard as
;; PostgreSQL 11 natively supports.

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

    [(column e)
     (recur e)]

    [(table e)
     (recur e)]

    [(placeholder v)
     (~a "$" (track-placeholder! v))]

    [(ident 'bitwise-not)     "~"]
    [(ident 'bitwise-and)     "&"]
    [(ident 'bitwise-or )     "|"]
    [(ident 'bitwise-xor)     "#"]
    [(ident 'is-distinct)     "IS DISTINCT"]
    [(ident 'is-not-distinct) "IS NOT DISTINCT"]
    [(ident 'is-not)          "IS NOT"]
    [(ident 'not-in)          "NOT IN"]
    [(ident 'not-like)        "NOT LIKE"]
    [(ident 'not-similar-to)  "NOT SIMILAR TO"]
    [(ident 'similar-to)      "SIMILAR TO"]

    [(ident n)
     (string-upcase (symbol->string n))]

    [(scalar #t) "TRUE"]
    [(scalar #f) "FALSE"]

    [(scalar (and (? list?) v))
     (~a "(" (string-join (map recur v) ", ") ")")]

    [(scalar (and (? string?) v))
     (~a "'" (string-replace v "'" "''") "'")]

    [(scalar v)
     (~v v)]

    [(qualified parent name)
     (~a (recur parent) "." (quote/standard name))]

    [(as e alias)
     (~a (maybe-parenthize e) " AS " (quote/standard alias))]

    [(app (and (ident (or
                       ;; logical ops: https://www.postgresql.org/docs/current/functions-logical.html
                       'not

                       ;; date ops: https://www.postgresql.org/docs/9.1/functions-datetime.html
                       'date 'time 'timestamp 'interval
                       ))
               op)
          (list a))
     (~a (recur op) " " (maybe-parenthize a))]

    [(app (ident 'cast) (list a b))
     (~a "CAST(" (maybe-parenthize a) " AS " (recur b) ")")]

    [(app (ident 'extract) (list a b))
     (~a "EXTRACT(" (recur a) " FROM " (maybe-parenthize b) ")")]

    [(app (and (ident (or
                       ;; bitwise ops: https://www.postgresql.org/docs/current/functions-math.html
                       'bitwise-not 'bitwise-and 'bitwise-or 'bitwise-xor '<< '>>

                       ;; logical ops: https://www.postgresql.org/docs/current/functions-logical.html
                       'and 'or

                       ;; comparison ops: https://www.postgresql.org/docs/current/functions-comparison.html
                       '= '> '< '>= '<= '<> '!= 'like 'not-like 'in 'not-in 'is 'is-not 'is-distinct 'is-not-distinct

                       ;; math ops: https://www.postgresql.org/docs/current/functions-math.html
                       '+ '- '* '/ '% '<< '>> '~ '~* '!~ '!~*

                       ;; string ops: https://www.postgresql.org/docs/current/functions-string.html
                       'similar-to 'not-similar-to
                       ))
               op)
          (list a b))
     (~a (maybe-parenthize a) " " (recur op) " " (maybe-parenthize b))]

    [(app (ident 'between) (list a b c))
     (~a (maybe-parenthize a) " BETWEEN " (maybe-parenthize b) " AND " (maybe-parenthize c))]

    [(app (ident 'position) (list a b))
     (~a "POSITION(" (maybe-parenthize a) " IN " (maybe-parenthize b) ")")]

    [(app (ident 'trim) (list a b c))
     (~a "TRIM(" (maybe-parenthize a) " " (maybe-parenthize b) " FROM " (maybe-parenthize c) ")")]

    [(app f args)
     (~a (recur f) "(" (string-join (map recur args) ", ") ")")]

    [(case-e cases else-case)
     (with-output-to-string
       (lambda _
         (display "CASE")
         (for ([c (in-list cases)])
           (display @~a{ WHEN @(recur (car c)) THEN @(recur (cdr c))}))
         (when else-case
           (display @~a{ ELSE @(recur else-case)}))
         (display " END")))]))

(define ((make-stmt-emitter recur emit-expr
                            #:supports-returning? [supports-returning? #f]) e)
  (match e
    [(list exprs ...)
     (string-join (map emit-expr exprs) ", ")]

    [(select columns from where group-by order-by offset limit)
     (with-output-to-string
       (lambda _
         (define columns:str
           (if (null? columns)
               "*"
               (recur columns)))

         (display (~a "SELECT " columns:str))
         (when from     (display (~a " " (recur from))))
         (when where    (display (~a " " (recur where))))
         (when group-by (display (~a " " (recur group-by))))
         (when order-by (display (~a " " (recur order-by))))
         (when limit    (display (~a " " (recur limit))))
         (when offset   (display (~a " " (recur offset))))))]

    [(update table assignments where returning)
     (with-output-to-string
       (lambda _
         (display @~a{UPDATE @(emit-expr table)})
         (when assignments (display (~a " " (recur assignments))))
         (when where       (display (~a " " (recur where))))
         (when (and returning supports-returning?)
           (display @~a{ RETURNING @(recur returning)}))))]

    [(delete from where)
     @~a{DELETE @(recur from) @(recur where)}]

    [(insert table columns column-values returning)
     (with-output-to-string
       (lambda _
         (display @~a{INSERT INTO @(emit-expr table) (@(recur columns))})
         (display @~a{ VALUES (@(recur column-values))})
         (when (and returning supports-returning?)
           (display @~a{ RETURNING @(recur returning)}))))]

    [(assignments pairs)
     (define pair:strs
       (for/list ([pair (in-list pairs)])
         @~a{@(emit-expr (car pair)) = @(emit-expr (cdr pair))}))

     @~a{SET @(string-join pair:strs ", ")}]

    [(limit n)        @~a{LIMIT @n}]
    [(from t)         @~a{FROM @(emit-expr t)}]
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
