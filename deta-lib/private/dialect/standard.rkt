#lang racket/base

(require racket/match
         racket/sequence
         racket/string
         "../ast.rkt"
         "dialect.rkt")

;; Implements a "standard" emitter for SQL from our AST.  I say
;; "standard", but what I really mean is as close to the standard as
;; PostgreSQL 11 natively supports.

(provide
 quote/standard
 make-expr-emitter
 make-stmt-emitter)

(define lowercase-alphanum-re
  #rx"[a-z0-9_]+")

(define (quote/standard e)
  (cond
    [(symbol? e) (symbol->string e)]
    [(regexp-match-exact? lowercase-alphanum-re e) e]
    [else (string-append "\"" e "\"")]))

(define ((make-expr-emitter display/expr) e)
  (define (display/maybe-parenthize e)
    (cond
      [(expr-terminal? e)
       (display/expr e)]

      [else
       (display "(")
       (display/expr e)
       (display ")")]))

  (define (display/quoted e)
    (display (quote/standard e)))

  (define (display/space e)
    (display " ")
    (display/expr e))

  (define (display/spaced e)
    (display " ")
    (display/expr e)
    (display " "))

  (match e
    [(? string?)
     (display (quote/standard e))]

    [(table e)  (display/expr e)]
    [(column e) (display/expr e)]

    [(placeholder v)
     (display "$")
     (display (track-placeholder! v))]

    [(ident 'array-concat)    (display "||")]
    [(ident 'array-contains?) (display "@>")]
    [(ident 'array-overlap?)  (display "&&")]
    [(ident 'bitwise-not)     (display "~")]
    [(ident 'bitwise-and)     (display "&")]
    [(ident 'bitwise-or )     (display "|")]
    [(ident 'bitwise-xor)     (display "#")]
    [(ident 'is-distinct)     (display "IS DISTINCT")]
    [(ident 'similar-to)      (display "SIMILAR TO")]
    [(ident name)             (display (string-upcase (symbol->string name)))]

    [(scalar #t) (display "TRUE")]
    [(scalar #f) (display "FALSE")]

    [(scalar (and (? list?) items))
     (display "(")
     (display/sep items display/expr)
     (display ")")]

    [(scalar (and (? string?) str))
     (display "'")
     (display (string-replace str "'" "''"))
     (display "'")]

    [(scalar (and (? vector?) v))
     (display "ARRAY[")
     (display/sep v display/expr)
     (display "]")]

    [(scalar v)
     (display v)]

    [(qualified parent name)
     (display/expr parent)
     (display ".")
     (display/quoted name)]

    [(as e alias)
     (display/maybe-parenthize e)
     (display " AS ")
     (display/quoted alias)]

    [(app (and (ident (or
                       ;; bitwise ops: https://www.postgresql.org/docs/current/functions-math.html
                       'bitwise-not

                       ;; logical ops: https://www.postgresql.org/docs/current/functions-logical.html
                       'not

                       ;; date ops: https://www.postgresql.org/docs/9.1/functions-datetime.html
                       'date 'time 'timestamp 'interval
                       ))
               op)
          (list a))
     (display/expr op)
     (display " ")
     (display/maybe-parenthize a)]

    [(app (ident 'cast) (list a b))
     (display "CAST(")
     (display/maybe-parenthize a)
     (display " AS ")
     (display/expr b)
     (display ")")]

    [(app (ident 'extract) (list a b))
     (display "EXTRACT(")
     (display/expr a)
     (display " FROM ")
     (display/maybe-parenthize b)
     (display ")")]

    [(app (and (ident (or
                       ;; array ops: https://www.postgresql.org/docs/current/functions-array.html
                       'array-concat 'array-contains? 'array-overlap?

                       ;; bitwise ops: https://www.postgresql.org/docs/current/functions-math.html
                       'bitwise-and 'bitwise-or 'bitwise-xor '<< '>>

                       ;; logical ops: https://www.postgresql.org/docs/current/functions-logical.html
                       'and 'or

                       ;; comparison ops: https://www.postgresql.org/docs/current/functions-comparison.html
                       '= '> '< '>= '<= '<> '!= 'ilike 'like 'in 'is 'is-distinct

                       ;; math ops: https://www.postgresql.org/docs/current/functions-math.html
                       '+ '- '* '/ '%

                       ;; string ops: https://www.postgresql.org/docs/current/functions-string.html
                       'similar-to
                       ))
               op)
          (list a b))
     (display/maybe-parenthize a)
     (display/spaced op)
     (display/maybe-parenthize b)]

    [(app (ident 'array-ref) (list a b))
     (display "(")
     (display/expr a)
     (display ")[")
     (display/maybe-parenthize b)
     (display "]")]

    [(app (ident 'array-slice) (list a b c))
     (display "(")
     (display/expr a)
     (display ")[")
     (display/maybe-parenthize b)
     (display ":")
     (display/maybe-parenthize c)
     (display "]")]

    [(app (ident 'between) (list a b c))
     (display/maybe-parenthize a)
     (display " BETWEEN ")
     (display/maybe-parenthize b)
     (display " AND ")
     (display/maybe-parenthize c)]

    [(app (ident 'position) (list a b))
     (display "POSITION(")
     (display/maybe-parenthize a)
     (display " IN ")
     (display/maybe-parenthize b)
     (display ")")]

    [(app (ident 'trim) (list a b c))
     (display "TRIM(")
     (display/maybe-parenthize a)
     (display " ")
     (display/maybe-parenthize b)
     (display " FROM ")
     (display/maybe-parenthize c)
     (display ")")]

    [(app f args)
     (display/expr f)
     (display "(")
     (display/sep args display/expr)
     (display ")")]

    [(case-e cases else-case)
     (display "CASE")

     (for ([c (in-list cases)])
       (display " WHEN ")
       (display/expr (car c))
       (display " THEN ")
       (display/expr (cdr c)))

     (when else-case
       (display " ELSE ")
       (display/expr else-case))

     (display " END")]))

(define ((make-stmt-emitter display/stmt
                            display/expr
                            #:supports-returning? [supports-returning? #f]) e)

  (define (display/space e)
    (display " ")
    (display/stmt e))

  (define (display/parens e)
    (display "(")
    (display/stmt e)
    (display ")"))

  (match e
    [(list exprs ...)
     (display/sep exprs display/expr)]

    [(select columns from where group-by order-by offset limit)
     (display "SELECT ")
     (cond
       [(null? columns) (display "*")]
       [else            (display/stmt columns)])

     (when from     (display/space from))
     (when where    (display/space where))
     (when group-by (display/space group-by))
     (when order-by (display/space order-by))
     (when limit    (display/space limit))
     (when offset   (display/space offset))]

    [(update table assignments where returning)
     (display "UPDATE ")
     (display/expr table)

     (when assignments (display/space assignments))
     (when where       (display/space where))
     (when (and returning supports-returning?)
       (display/space returning))]

    [(delete from where returning)
     (display "DELETE ")
     (display/stmt from)
     (when where (display/space where))
     (when (and returning supports-returning?)
       (display/space returning))]

    [(insert table columns column-values returning)
     (display "INSERT INTO ")
     (display/expr table)
     (display/parens columns)
     (display " VALUES ")
     (display/parens column-values)
     (when (and returning supports-returning?)
       (display/space returning))]

    [(assignments pairs)
     (display "SET ")
     (display/sep
      pairs
      (match-lambda
        [(cons l r)
         (display/expr l)
         (display " = ")
         (display/expr r)]))]

    [(limit n)
     (display "LIMIT ")
     (display n)]

    [(from t)
     (display "FROM ")
     (display/expr t)]

    [(where e)
     (display "WHERE ")
     (display/expr e)]

    [(group-by cols)
     (display "GROUP BY ")
     (display/stmt cols)]

    [(offset n)
     (display "OFFSET ")
     (display n)]

    [(returning es)
     (display "RETURNING ")
     (display/stmt es)]

    [(order-by pairs)
     (display "ORDER BY ")
     (display/sep pairs
                  (match-lambda
                    [(cons e dir)
                     (display/expr e)
                     (when (eq? dir 'desc)
                       (display " DESC"))]))]))

(define (display/sep xs display-p
                     #:sep [sep ", "])
  (define n-xs
    (sequence-length xs))

  (for ([i (in-naturals 1)]
        [x xs])
    (display-p x)
    (unless (= i n-xs)
      (display sep))))