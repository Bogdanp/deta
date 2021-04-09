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
     #:with (op-str ...) (for/list ([op (in-list (syntax-e #'(op ...)))]
                                    [maybe-str (in-list (syntax-e #'((~? maybe-op-str #f) ...)))])
                           (cond
                             [(syntax->datum maybe-str) maybe-str]
                             [else (datum->syntax op (string-upcase (symbol->string (syntax->datum op))))]))
     #:with write-operator-id (format-id stx "write-~a-operator" #'kind)
     #:with match-expander-id (format-id stx "~a-operator" #'kind)
     (syntax/loc stx
       (begin
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
               [(_ out) #'(and (or 'op ...) out)])))))]))

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
  [json-check-path "@@"]
  [json-contains-all? "?&"]
  [json-contains-any? "?|"]
  [json-contains-path? "@?"]
  [json-contains? "?"]
  [json-ref-text "->>"]
  [json-ref-text/path "#>>"]
  [json-subset? "<@"]
  [json-superset? "@>"]
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
  [json-concat "||"]
  [json-ref "->"]
  [json-ref/path "#>"]
  [json-remove "-"]
  [json-remove/path "#-"]
  [or]
  [string-concat "||"])
