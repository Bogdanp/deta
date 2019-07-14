#lang racket/base

(require (for-syntax racket/base
                     racket/contract
                     racket/function
                     racket/list
                     racket/provide-transform
                     racket/struct-info
                     racket/syntax
                     syntax/parse
                     syntax/parse/experimental/template)
         db
         racket/contract
         racket/function
         racket/match
         "private/field.rkt"
         "private/meta.rkt"
         "private/type.rkt")

;; schema ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 entity?
 entity-meta

 define-schema
 schema?
 schema-name
 schema-table-name
 schema-virtual?
 schema-struct-ctor
 schema-struct-pred
 schema-meta-updater
 schema-fields
 schema-primary-key
 schema-out)

(struct entity (meta)
  #:transparent)

(struct schema
  (name
   table-name
   virtual?
   struct-ctor
   struct-pred
   meta-updater
   fields)
  #:transparent)

(define (make-schema #:name name
                     #:table-name table-name
                     #:virtual? virtual?
                     #:struct-ctor struct-ctor
                     #:struct-pred struct-pred
                     #:meta-updater meta-updater
                     #:fields fields)

  (define the-schema
    (schema name
            table-name
            virtual?
            struct-ctor
            struct-pred
            meta-updater
            fields))

  (begin0 the-schema
    (unless virtual?
      (register! name the-schema))))

(define/contract (schema-primary-key schema)
  (-> schema? (or/c false/c field?))
  (for/first ([f (in-list (schema-fields schema))]
              #:when (field-primary-key? f))
    f))

(begin-for-syntax
  (define (pluralize name)
    (format "~as" name))

  (define syntax->keyword
    (compose1 string->keyword symbol->string syntax->datum))

  (define-template-metafunction (make-fld-setter stx)
    (syntax-parse stx
      [(_ struct-name struct-pred fld-name fld-contract fld-wrapper)
       (with-syntax ([setter-name (format-id #'struct-name "set-~a-~a" #'struct-name #'fld-name)])
         #'(define/contract (setter-name e v [track-change? #t])
             (->* (struct-pred fld-contract) (boolean?) struct-pred)
             (define meta
               (if track-change?
                   (meta-track-change (entity-meta e) 'fld-name)
                   (entity-meta e)))

             (struct-copy struct-name e
                          [meta #:parent entity meta]
                          [fld-name (fld-wrapper v)])))]))

  (define-template-metafunction (make-fld-updater stx)
    (syntax-parse stx
      [(_ struct-name struct-pred fld-name fld-contract fld-wrapper)
       (with-syntax ([getter-name  (format-id #'struct-name "~a-~a"        #'struct-name #'fld-name)]
                     [updater-name (format-id #'struct-name "update-~a-~a" #'struct-name #'fld-name)])
         #'(define/contract (updater-name e p [track-change? #t])
             (->* (struct-pred (-> fld-contract fld-contract)) (boolean?) struct-pred)
             (define meta
               (if track-change?
                   (meta-track-change (entity-meta e) 'fld-name)
                   (entity-meta e)))

             (struct-copy struct-name e
                          [meta #:parent entity meta]
                          [fld-name (fld-wrapper (p (getter-name e)))])))]))

  (define-template-metafunction (make-fld-maker stx)
    (syntax-parse stx
      [(_ struct-name fld-name fld-type fld-pk? fld-ai? fld-nullable? fld-unique?)
       (with-syntax ([getter-name  (format-id #'struct-name "~a-~a"        #'struct-name #'fld-name)]
                     [setter-name  (format-id #'struct-name "set-~a-~a"    #'struct-name #'fld-name)]
                     [updater-name (format-id #'struct-name "update-~a-~a" #'struct-name #'fld-name)])
         #'(make-field #:id 'fld-name
                       #:type fld-type
                       #:getter getter-name
                       #:setter setter-name
                       #:updater updater-name
                       #:primary-key? fld-pk?
                       #:auto-increment? fld-ai?
                       #:nullable? fld-nullable?
                       #:unique? fld-unique?))]))

  (define-template-metafunction (make-ctor-contract stx)
    (syntax-parse stx
      [(_ [(fld-kwd fld-contract fld-required?) ...] struct-pred)
       (with-syntax ([((required-arg ...) ...)
                      (filter-map (lambda (fld-kwd fld-contract fld-required?)
                                    (and (syntax->datum fld-required?)
                                         (quasisyntax/loc stx
                                           (#,fld-kwd #,fld-contract))))
                                  (syntax-e #'(fld-kwd ...))
                                  (syntax-e #'(fld-contract ...))
                                  (syntax-e #'(fld-required? ...)))]
                     [((optional-arg ...) ...)
                      (filter-map (lambda (fld-kwd fld-contract fld-required?)
                                    (and (not (syntax->datum fld-required?))
                                         (quasisyntax/loc stx
                                           (#,fld-kwd #,fld-contract))))
                                  (syntax-e #'(fld-kwd ...))
                                  (syntax-e #'(fld-contract ...))
                                  (syntax-e #'(fld-required? ...)))])
         #'(->* (required-arg ... ...)
                (optional-arg ... ...)
                struct-pred))]))

  (define-syntax-class fld
    (pattern (name:id type:expr (~alt (~optional (~and #:primary-key primary-key))
                                      (~optional (~and #:auto-increment auto-increment))
                                      (~optional (~and #:nullable nullable))
                                      (~optional (~and #:unique unique))
                                      (~optional (~seq #:contract contract-e:expr) #:defaults ([contract-e #'any/c]))
                                      (~optional (~seq #:wrapper wrapper:expr) #:defaults ([wrapper #'values]))) ...)
             #:fail-when (and (attribute primary-key)
                              (attribute nullable))
             "primary keys may not be nullable"

             #:with required? (if (or (attribute auto-increment)
                                      (attribute nullable)) #'#f #'t)
             #:with primary-key? (if (attribute primary-key) #'#t #'#f)
             #:with auto-increment? (if (attribute auto-increment) #'#t #'#f)
             #:with nullable? (if (attribute nullable) #'#t #'#f)
             #:with unique? (if (attribute unique) #'#t #'#f)
             #:with contract (if (attribute nullable)
                                 #'(or/c sql-null? (and/c (type-contract type) contract-e))
                                 #'(and/c (type-contract type) contract-e))
             #:with ctor-kwd (syntax->keyword #'name)
             #:with ctor-arg (cond
                               [(attribute primary-key) #'(ctor-kwd [name sql-null])]
                               [(attribute nullable)    #'(ctor-kwd [name sql-null])]
                               [else                    #'(ctor-kwd  name)]))

    (pattern ((name:id default:expr) type:expr (~alt (~optional (~and #:primary-key primary-key))
                                                     (~optional (~and #:auto-increment auto-increment))
                                                     (~optional (~and #:nullable nullable))
                                                     (~optional (~and #:unique unique))
                                                     (~optional (~seq #:contract contract-e:expr) #:defaults ([contract-e #'any/c]))
                                                     (~optional (~seq #:wrapper wrapper:expr) #:defaults ([wrapper #'values]))) ...)
             #:fail-when (and (attribute primary-key)
                              (attribute nullable))
             "primary keys may not be nullable"

             #:with required? #'#f
             #:with primary-key? (if (attribute primary-key) #'#t #'#f)
             #:with auto-increment? (if (attribute auto-increment) #'#t #'#f)
             #:with nullable? (if (attribute nullable) #'#t #'#f)
             #:with unique? (if (attribute unique) #'#t #'#f)
             #:with contract (if (attribute nullable)
                                 #'(or/c sql-null? (and/c (type-contract type) contract-e))
                                 #'(and/c (type-contract type) contract-e))
             #:with ctor-kwd (syntax->keyword #'name)
             #:with ctor-arg #'(ctor-kwd [name default]))))

(define-syntax (define-schema stx)
  (syntax-parse stx
    [(_ name:id
        (~alt (~optional (~seq #:table table-name:str))
              (~optional (~and #:virtual virtual))) ...
        (f:fld ...+))
     (with-syntax* ([pluralized-name (datum->syntax #'name (pluralize (syntax->datum #'name)))]
                    [table-name #'(~? table-name pluralized-name)]
                    [virtual? (if (attribute virtual) #'#t #'#f)]
                    [ctor-name (format-id #'name "make-~a" #'name)]
                    [((ctor-arg ...) ...) #'(f.ctor-arg ...)]
                    [meta-updater-name (format-id #'name "update-~a-meta" #'name)]
                    [struct-name #'name]
                    [struct-ctor-name (gensym)]
                    [struct-pred (format-id #'name "~a?" #'name)]
                    [schema-name (format-id #'name "~a-schema" #'name)])
       #'(begin
           (struct struct-name entity (f.name ...)
             #:constructor-name struct-ctor-name
             #:transparent)

           (define/contract (ctor-name ctor-arg ... ...)
             (make-ctor-contract [(f.ctor-kwd f.contract f.required?) ...] struct-pred)
             (struct-ctor-name (make-meta schema-name)
                               (f.wrapper f.name) ...))

           (define/contract (meta-updater-name e p)
             (-> entity? (-> meta? meta?) entity?)
             (struct-copy struct-name e [meta #:parent entity (p (entity-meta e))]))

           (begin (make-fld-setter  struct-name struct-pred f.name f.contract f.wrapper) ...)
           (begin (make-fld-updater struct-name struct-pred f.name f.contract f.wrapper) ...)

           (define schema-name
             (make-schema #:name 'name
                          #:table-name table-name
                          #:virtual? virtual?
                          #:struct-ctor ctor-name
                          #:struct-pred struct-pred
                          #:meta-updater meta-updater-name
                          #:fields (list (make-fld-maker struct-name
                                                         f.name
                                                         f.type
                                                         f.primary-key?
                                                         f.auto-increment?
                                                         f.nullable?
                                                         f.unique?) ...)))))]))

;; Heavily inspired from: https://github.com/racket/racket/blob/20e669f47842d47b085ddedc5782e4a95495653a/racket/collects/racket/private/reqprov.rkt#L978
(define-syntax schema-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ struct-name:id)
        (define id #'struct-name)
        (define v (syntax-local-value id (const #f)))
        (unless (struct-info? v)
          (raise-syntax-error #f "identifier is not bound to struct type information" stx id))

        (define info (extract-struct-info v))
        (define accessors/without-super
          (for/list ([stx (in-list (list-ref info 3))]
                     #:when (and stx (not (member (syntax->datum stx)
                                                  '(entity-meta)))))
            stx))
        (define accessors+setters+updaters
          (for/fold ([all null])
                    ([stx (in-list accessors/without-super)])
            (append
             (list stx
                   (format-id #'struct-name "set-~a" stx)
                   (format-id #'struct-name "update-~a" stx))
             all)))

        (define stxs
          (append
           (list
            (format-id #'struct-name "make-~a" #'struct-name)
            (format-id #'struct-name "~a-schema" #'struct-name)
            (list-ref info 0)   ;; struct descriptor
            (list-ref info 2))  ;; struct predicate
           accessors+setters+updaters))

        (for/list ([stx (in-list stxs)])
          (make-export stx (syntax-e stx) 0 #f stx))]))))


;; registry ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 schema-registry-ref
 schema-registry-lookup)

(define/contract schema-registry
  (parameter/c (hash/c symbol? schema?))
  (make-parameter (hasheq)))

(define (register! name schema)
  (define registry (schema-registry))
  (when (hash-has-key? registry name)
    (raise-user-error 'register! "schema ~a conflicts with a previous one" name))

  (schema-registry
   (hash-set registry name schema)))

(define/contract (schema-registry-ref name)
  (-> symbol? (or/c false/c schema?))
  (hash-ref (schema-registry) name #f))

(define/contract (schema-registry-lookup schema-or-name)
  (-> (or/c schema? symbol?) schema?)
  (define schema
    (match schema-or-name
      [(? schema?)                      schema-or-name ]
      [(? symbol?) (schema-registry-ref schema-or-name)]))

  (unless schema
    (raise-argument-error 'lookup-schema "unregistered schema" schema-or-name))

  schema)
