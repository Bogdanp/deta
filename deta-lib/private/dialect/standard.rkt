#lang racket/base

(require racket/match
         racket/port
         racket/sequence
         racket/string
         "../ast.rkt"
         "dialect.rkt"
         "operator.rkt")

;; Implements a "standard" emitter for SQL from our AST.  I say
;; "standard", but what I really mean is as close to the standard as
;; PostgreSQL 11 natively supports.

(provide
 quote/standard
 make-expr-emitter
 make-stmt-emitter
 write/sep)

(define (quote/standard e)
  (cond
    [(symbol? e) (symbol->string e)]
    [(string=? e "*") "*"]
    [(regexp-match-exact? #rx"[a-z_][a-z0-9_]*" e) e]
    [else (string-append "\"" e "\"")]))

(define ((make-expr-emitter write-expr write-stmt) e)
  (define (display/maybe-parenthize e)
    (match e
      [(app (ident 'any) _)
       (write-expr e)]

      [(? expr-terminal?)
       (write-expr e)]

      [_
       (write-string "(")
       (write-expr e)
       (write-string ")")]))

  (define (display/quoted e)
    (write-string (quote/standard e)))

  (match e
    [(? string?)
     (write-string (quote/standard e))]

    [(table e)  (write-expr e)]
    [(column e) (write-expr e)]

    [(ident i)
     (write-string (ident->string i))]

    [(placeholder v)
     (write-string "$")
     (display (track-placeholder! v))]

    [(scalar #t) (write-string "TRUE")]
    [(scalar #f) (write-string "FALSE")]

    [(scalar (and (? list?) items))
     (write-string "(")
     (write/sep items write-expr)
     (write-string ")")]

    [(scalar (and (? string?) str))
     (write-string "'")
     (write-string (string-replace str "'" "''"))
     (write-string "'")]

    [(scalar (and (? vector?) v))
     (write-string "ARRAY[")
     (write/sep v write-expr)
     (write-string "]")]

    [(scalar v)
     (display v)]

    [(qualified parent name)
     (write-expr parent)
     (write-string ".")
     (display/quoted name)]

    [(as e alias)
     (display/maybe-parenthize e)
     (write-string " AS ")
     (display/quoted alias)]

    [(app (ident (unary-operator op)) (list a))
     (write-unary-operator op)
     (write-string " ")
     (display/maybe-parenthize a)]

    [(app (ident (binary-operator 'array-ref)) (list a b))
     (write-string "(")
     (write-expr a)
     (write-string ")[")
     (display/maybe-parenthize b)
     (write-string "]")]

    [(app (ident (binary-operator 'cast)) (list a b))
     (write-string "CAST(")
     (display/maybe-parenthize a)
     (write-string " AS ")
     (write-expr b)
     (write-string ")")]

    [(app (ident (binary-operator 'extract)) (list a b))
     (write-string "EXTRACT(")
     (write-expr a)
     (write-string " FROM ")
     (display/maybe-parenthize b)
     (write-string ")")]

    [(app (ident (binary-operator 'position)) (list a b))
     (write-string "POSITION(")
     (display/maybe-parenthize a)
     (write-string " IN ")
     (display/maybe-parenthize b)
     (write-string ")")]

    [(app (ident (binary-operator op)) (list a b))
     (display/maybe-parenthize a)
     (write-string " ")
     (write-binary-operator op)
     (write-string " ")
     (display/maybe-parenthize b)]

    [(app (ident (ternary-operator 'array-slice)) (list a b c))
     (write-string "(")
     (write-expr a)
     (write-string ")[")
     (display/maybe-parenthize b)
     (write-string ":")
     (display/maybe-parenthize c)
     (write-string "]")]

    [(app (ident (ternary-operator 'between)) (list a b c))
     (display/maybe-parenthize a)
     (write-string " BETWEEN ")
     (display/maybe-parenthize b)
     (write-string " AND ")
     (display/maybe-parenthize c)]

    [(app (ident (ternary-operator 'trim)) (list a b c))
     (write-string "TRIM(")
     (display/maybe-parenthize a)
     (write-string " ")
     (display/maybe-parenthize b)
     (write-string " FROM ")
     (display/maybe-parenthize c)
     (write-string ")")]

    [(app (ident (variadic-operator op)) es)
     (define separator
       (with-output-to-string
         (lambda ()
           (display " ")
           (write-variadic-operator op)
           (display " "))))

     (write/sep
      #:sep separator
      es display/maybe-parenthize)]

    [(app (ident (unary-operator op)) args)
     (apply raise-arity-error op 1 args)]

    [(app (ident (binary-operator op)) args)
     (apply raise-arity-error op 2 args)]

    [(app (ident (ternary-operator op)) args)
     (apply raise-arity-error op 3 args)]

    [(app f args)
     (write-expr f)
     (write-string "(")
     (write/sep args write-expr)
     (write-string ")")]

    [(case-e cases else-case)
     (write-string "CASE")

     (for ([c (in-list cases)])
       (write-string " WHEN ")
       (write-expr (car c))
       (write-string " THEN ")
       (write-expr (cdr c)))

     (when else-case
       (write-string " ELSE ")
       (write-expr else-case))

     (write-string " END")]

    [(subquery stmt)
     (write-string "(")
     (write-stmt stmt)
     (write-string ")")]))

(define ((make-stmt-emitter write-stmt
                            write-expr
                            #:supports-returning? [supports-returning? #f]) e)

  (define (display/space e)
    (write-string " ")
    (write-stmt e))

  (define (display/parens e)
    (write-string "(")
    (write-stmt e)
    (write-string ")"))

  (match e
    [(? expr?)
     (write-expr e)]

    [(list exprs ...)
     (write/sep exprs write-expr)]

    [(select distinct? columns from where group-by union order-by offset limit)
     (write-string "SELECT ")
     (when distinct?
       (write-string "DISTINCT "))
     (cond
       [(null? columns) (write-string "*")]
       [else            (write-stmt columns)])

     (when from     (display/space from))
     (when where    (display/space where))
     (when group-by (display/space group-by))
     (when union    (display/space union))
     (when order-by (display/space order-by))
     (when limit    (display/space limit))
     (when offset   (display/space offset))]

    [(update table assignments where returning)
     (write-string "UPDATE ")
     (write-expr table)

     (when assignments (display/space assignments))
     (when where       (display/space where))
     (when (and returning supports-returning?)
       (display/space returning))]

    [(delete from where returning)
     (write-string "DELETE ")
     (write-stmt from)
     (when where (display/space where))
     (when (and returning supports-returning?)
       (display/space returning))]

    [(insert table columns column-values returning)
     (write-string "INSERT INTO ")
     (write-expr table)
     (display/parens columns)
     (write-string " VALUES ")
     (display/parens column-values)
     (when (and returning supports-returning?)
       (display/space returning))]

    [(assignments pairs)
     (write-string "SET ")
     (write/sep
      pairs
      (match-lambda
        [(cons l r)
         (write-expr l)
         (write-string " = ")
         (write-expr r)]))]

    [(limit e)
     (write-string "LIMIT ")
     (write-expr e)]

    [(from tables joins)
     (write-string "FROM ")
     (write/sep tables write-stmt)
     (unless (null? joins)
       (for ([join (in-list joins)])
         (write-stmt join)))]

    [(join type lateral? with constraint)
     (write-string " ")
     (write-string (join-type->string type))
     (write-string " ")
     (when lateral?
       (write-string "LATERAL "))
     (write-expr with)
     (write-string " ON ")
     (write-expr constraint)]

    [(where e)
     (write-string "WHERE ")
     (write-expr e)]

    [(group-by cols)
     (write-string "GROUP BY ")
     (write-stmt cols)]

    [(offset e)
     (write-string "OFFSET ")
     (write-expr e)]

    [(returning es)
     (write-string "RETURNING ")
     (write-stmt es)]

    [(order-by orderings)
     (write-string "ORDER BY ")
     (write/sep
      orderings
      (match-lambda
        [(list e dir nulls-dir)
         (write-expr e)
         (when (eq? dir 'desc)
           (write-string " DESC"))
         (when nulls-dir
           (write-string " NULLS")
           (case nulls-dir
             [(nulls-first) (write-string " FIRST")]
             [(nulls-last) (write-string " LAST")]))]))]

    [(union stmt)
     (write-string "UNION (")
     (write-stmt stmt)
     (write-string ")")]))

(define (write/sep xs write-proc
                   #:sep [sep ", "])
  (define n-xs
    (sequence-length xs))

  (for ([i (in-naturals 1)]
        [x xs])
    (write-proc x)
    (unless (= i n-xs)
      (write-string sep))))

(define (ident->string s)
  (string-upcase (symbol->string s)))

(define (join-type->string t)
  (case t
    [(inner)       "JOIN"]
    [(left)   "LEFT JOIN"]
    [(right) "RIGHT JOIN"]
    [(full)   "FULL JOIN"]
    [(cross) "CROSS JOIN"]))
