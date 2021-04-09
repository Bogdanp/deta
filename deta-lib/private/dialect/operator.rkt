#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/match
         syntax/parse
         syntax/parse/define)

(provide
 define-ops)

(define-syntax (define-ops stx)
  (syntax-parse stx
    [(_ kind:id [op:id (~optional maybe-op-str:str)] ...)
     #:with (op-id ...) #'(op ...)
     #:with (op-str ...) (for/list ([op (in-list (syntax-e #'(op ...)))]
                                    [maybe-str (in-list (syntax-e #'((~? maybe-op-str #f) ...)))])
                           (cond
                             [(syntax->datum maybe-str) maybe-str]
                             [else (datum->syntax op (string-upcase (symbol->string (syntax->datum op))))]))
     #:with write-operator-id (format-id stx "write-~a-operator" #'kind)
     #:with match-expander-id (format-id stx "~a-operator" #'kind)
     #:with literal-set-id (format-id stx "~a-ops" #'kind)
     #:with syntax-class-id (format-id stx "~a-op" #'kind)
     (syntax/loc stx
       (begin
         (provide op ...)
         (define-syntax (op stx)
           (raise-syntax-error 'op "cannot be used outside of a query" stx)) ...
         (module+ private
           (provide
            write-operator-id
            match-expander-id)
           (define (write-operator-id id)
             (write-string
              (case id
                [(op) op-str] ...)))
           (define-match-expander match-expander-id
             (lambda (stx)
               (syntax-parse stx
                 [(_) #'(or 'op ...)]
                 [(_ out) #'(and (or 'op ...) out)]))))
         (module+ syntax
           (provide
            (for-syntax syntax-class-id))
           (begin-for-syntax
             (define-literal-set literal-set-id
               (op ...))
             (define-syntax-class syntax-class-id
               #:literal-sets ([literal-set-id])
               (pattern op #:with e #''op-id) ...)))))]))

(define-ops unary
  [bitwise-not "~"]
  [date]
  [interval]
  [json]
  [jsonb]
  [not]
  [time]
  [timestamp])

(define-ops binary
  [=] [>] [<] [>=] [<=] [<>] [!=]
  [array-contains? "@>"]
  [array-overlap? "&&"]
  [array-ref]
  [cast]
  [extract]
  [ilike]
  [in]
  [is-distinct "IS DISTINCT"]
  [is]
  [like]
  [position]
  [similar-to "SIMILAR TO"])

(define-ops ternary
  [array-slice]
  [between]
  [trim])

(define-ops variadic
  [+] [-] [*] [/] [%]
  [<<] [>>]
  [and]
  [array-concat "||"]
  [bitwise-and "&"]
  [bitwise-or "|"]
  [bitwise-xor "#"]
  [json-ref "->"]
  [json-ref-text "->>"]
  [json-ref-text/path "#>>"]
  [json-ref/path "#>"]
  [or])
