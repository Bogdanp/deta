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
         "private/entity.rkt"
         "private/field.rkt"
         "private/meta.rkt"
         "private/schema.rkt"
         "private/type.rkt")

;; schema ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 entity?
 entity-meta

 define-schema
 schema?
 schema-out)

;; Terms:
;;  * anything ending in *id refers to a binding
;;  * anything ending in *name refers to that thing's name in the database table
(begin-for-syntax
  (define (pluralize name)
    (format "~as" name))

  (define syntax->keyword
    (compose1 string->keyword symbol->string syntax->datum))

  (define-template-metafunction (make-fld-setter stx)
    (syntax-parse stx
      [(_ struct-id struct-pred-id fld-id fld-contract fld-wrapper)
       (with-syntax ([setter-id (format-id #'struct-id "set-~a-~a" #'struct-id #'fld-id)])
         #'(define/contract (setter-id e v [track-change? #t])
             (->* (struct-pred-id fld-contract) (boolean?) struct-pred-id)
             (define meta
               (if track-change?
                   (meta-track-change (entity-meta e) 'fld-id)
                   (entity-meta e)))

             (struct-copy struct-id e
                          [meta #:parent entity meta]
                          [fld-id (fld-wrapper v)])))]))

  (define-template-metafunction (make-fld-updater stx)
    (syntax-parse stx
      [(_ struct-id struct-pred-id fld-id fld-contract fld-wrapper)
       (with-syntax ([getter-id  (format-id #'struct-id "~a-~a"        #'struct-id #'fld-id)]
                     [updater-id (format-id #'struct-id "update-~a-~a" #'struct-id #'fld-id)])
         #'(define/contract (updater-id e p [track-change? #t])
             (->* (struct-pred-id (-> fld-contract fld-contract)) (boolean?) struct-pred-id)
             (define meta
               (if track-change?
                   (meta-track-change (entity-meta e) 'fld-id)
                   (entity-meta e)))

             (struct-copy struct-id e
                          [meta #:parent entity meta]
                          [fld-id (fld-wrapper (p (getter-id e)))])))]))

  (define-template-metafunction (make-fld-maker stx)
    (syntax-parse stx
      [(_ struct-id fld-id fld-name fld-type fld-pk? fld-ai? fld-nullable? fld-unique? fld-virtual?)
       (with-syntax ([getter-id  (format-id #'struct-id "~a-~a"        #'struct-id #'fld-id)]
                     [setter-id  (format-id #'struct-id "set-~a-~a"    #'struct-id #'fld-id)]
                     [updater-id (format-id #'struct-id "update-~a-~a" #'struct-id #'fld-id)])
         #'(make-field #:id 'fld-id
                       #:name fld-name
                       #:type fld-type
                       #:getter getter-id
                       #:setter setter-id
                       #:updater updater-id
                       #:primary-key? fld-pk?
                       #:auto-increment? fld-ai?
                       #:nullable? fld-nullable?
                       #:unique? fld-unique?
                       #:virtual? fld-virtual?))]))

  (define-template-metafunction (make-ctor-contract stx)
    (syntax-parse stx
      [(_ [(fld-kwd fld-contract fld-required?) ...] struct-pred-id)
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
                struct-pred-id))]))

  (define-syntax-class fld
    (pattern (id:id type:expr (~alt (~optional (~and #:primary-key primary-key))
                                    (~optional (~and #:auto-increment auto-increment))
                                    (~optional (~and #:nullable nullable))
                                    (~optional (~and #:unique unique))
                                    (~optional (~seq #:name name-e:str))
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
             #:with virtual? #'#f
             #:with name (if (attribute name-e)
                             #'name-e
                             #'(id->column-name 'id))
             #:with contract (if (attribute nullable)
                                 #'(or/c sql-null? (and/c (type-contract type) contract-e))
                                 #'(and/c (type-contract type) contract-e))
             #:with ctor-kwd (syntax->keyword #'id)
             #:with ctor-arg (cond
                               [(attribute primary-key) #'(ctor-kwd [id sql-null])]
                               [(attribute nullable)    #'(ctor-kwd [id sql-null])]
                               [else                    #'(ctor-kwd  id)]))

    (pattern ((id:id default:expr) type:expr (~alt (~optional (~and #:primary-key primary-key))
                                                   (~optional (~and #:auto-increment auto-increment))
                                                   (~optional (~and #:nullable nullable))
                                                   (~optional (~and #:unique unique))
                                                   (~optional (~and #:virtual virtual))
                                                   (~optional (~seq #:name name-e:str))
                                                   (~optional (~seq #:contract contract-e:expr) #:defaults ([contract-e #'any/c]))
                                                   (~optional (~seq #:wrapper wrapper:expr) #:defaults ([wrapper #'values]))) ...)
             #:fail-when (and (attribute primary-key)
                              (attribute nullable))
             "primary keys may not be nullable"

             #:fail-when (and (attribute virtual)
                              (or (attribute primary-key)
                                  (attribute auto-increment)
                                  (attribute nullable)
                                  (attribute unique)
                                  (attribute name-e)))
             "virtual fields may not have database-related attributes"

             #:with required? #'#f
             #:with primary-key? (if (attribute primary-key) #'#t #'#f)
             #:with auto-increment? (if (attribute auto-increment) #'#t #'#f)
             #:with nullable? (if (attribute nullable) #'#t #'#f)
             #:with unique? (if (attribute unique) #'#t #'#f)
             #:with virtual? (if (attribute virtual) #'#t #'#f)
             #:with name (if (attribute name-e)
                             #'name-e
                             #'(id->column-name 'id))
             #:with contract (if (attribute nullable)
                                 #'(or/c sql-null? (and/c (type-contract type) contract-e))
                                 #'(and/c (type-contract type) contract-e))
             #:with ctor-kwd (syntax->keyword #'id)
             #:with ctor-arg #'(ctor-kwd [id default]))))

(define-syntax (define-schema stx)
  (syntax-parse stx
    [(_ struct-id:id
        (~alt (~optional (~seq #:table table-name:str))
              (~optional (~and #:virtual virtual))) ...
        (f:fld ...+)
        (~alt (~optional (~seq #:pre-persist-hook pre-persist-hook-e:expr) #:defaults ([pre-persist-hook-e #'values]))
              (~optional (~seq #:pre-delete-hook pre-delete-hook-e:expr)   #:defaults ([pre-delete-hook-e  #'values]))) ...
        struct-option ...)

     #:fail-when (> (length (filter values (syntax->datum #'(f.primary-key? ...)))) 1)
     "at most one field can be marked as a #:primary-key"

     (with-syntax* ([pluralized-name (datum->syntax #'struct-id (pluralize (syntax->datum #'struct-id)))]
                    [table-name #'(~? table-name pluralized-name)]
                    [virtual? (if (attribute virtual) #'#t #'#f)]
                    [ctor-id (format-id #'struct-id "make-~a" #'struct-id)]
                    [((ctor-arg ...) ...) #'(f.ctor-arg ...)]
                    [meta-updater-id (format-id #'struct-id "update-~a-meta" #'struct-id)]
                    [struct-ctor-id (gensym)]
                    [struct-pred-id (format-id #'struct-id "~a?" #'struct-id)]
                    [schema-id (format-id #'struct-id "~a-schema" #'struct-id)])
       #'(begin
           (struct struct-id entity (f.id ...)
             #:constructor-name struct-ctor-id
             #:transparent
             struct-option ...)

           (define/contract (ctor-id ctor-arg ... ...)
             (make-ctor-contract [(f.ctor-kwd f.contract f.required?) ...] struct-pred-id)
             (struct-ctor-id (make-meta schema-id)
                             (f.wrapper f.id) ...))

           (define/contract (meta-updater-id e p)
             (-> entity? (-> meta? meta?) entity?)
             (struct-copy struct-id e [meta #:parent entity (p (entity-meta e))]))

           (begin (make-fld-setter  struct-id struct-pred-id f.id f.contract f.wrapper) ...)
           (begin (make-fld-updater struct-id struct-pred-id f.id f.contract f.wrapper) ...)

           (define schema-id
             (make-schema #:id 'struct-id
                          #:table table-name
                          #:virtual? virtual?
                          #:struct-ctor ctor-id
                          #:struct-pred struct-pred-id
                          #:meta-updater meta-updater-id
                          #:pre-persist-hook (contract
                                              (-> struct-pred-id struct-pred-id)
                                              pre-persist-hook-e
                                              'struct-id 'struct-id
                                              'pre-persist-hook #f)
                          #:pre-delete-hook (contract
                                             (-> struct-pred-id struct-pred-id)
                                             pre-delete-hook-e
                                             'struct-id 'struct-id
                                             'pre-delete-hook #f)
                          #:fields (list (make-fld-maker struct-id
                                                         f.id
                                                         f.name
                                                         f.type
                                                         f.primary-key?
                                                         f.auto-increment?
                                                         f.nullable?
                                                         f.unique?
                                                         f.virtual?) ...)))))]))

;; Heavily inspired from:
;; https://github.com/racket/racket/blob/20e669f47842d47b085ddedc5782e4a95495653a/racket/collects/racket/private/reqprov.rkt#L978
(define-syntax schema-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ struct-id:id)
        (define v (syntax-local-value #'struct-id (const #f)))
        (unless (struct-info? v)
          (raise-syntax-error #f "identifier is not bound to struct type information" stx #'struct-id))

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
                   (format-id #'struct-id "set-~a" stx)
                   (format-id #'struct-id "update-~a" stx))
             all)))

        (define stxs
          (append
           (list
            #'struct-id
            (format-id #'struct-id "make-~a" #'struct-id)
            (format-id #'struct-id "~a-schema" #'struct-id)
            (list-ref info 0)
            (list-ref info 2))
           accessors+setters+updaters))

        (for/list ([stx (in-list stxs)])
          (make-export stx (syntax-e stx) 0 #f stx))]))))
