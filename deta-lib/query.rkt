#lang racket/base

(require (for-syntax racket/base
                     racket/match
                     racket/string
                     syntax/parse
                     "private/field.rkt")
         (except-in db
                    query)
         racket/contract
         racket/match
         racket/sequence
         racket/set
         (prefix-in ast: "private/ast.rkt")
         "private/connection.rkt"
         "private/dialect/dialect.rkt"
         "private/entity.rkt"
         "private/field.rkt"
         "private/meta.rkt"
         (prefix-in dyn: "private/query.rkt")
         "private/schema.rkt"
         "private/type.rkt")

;; ddl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 create-table!
 drop-table!)

(define/contract (create-table! conn schema-or-name)
  (-> connection? (or/c schema? symbol?) void?)
  (define schema (schema-registry-lookup schema-or-name))
  (query-exec conn (dialect-emit-ddl (connection-dialect conn)
                                     (ast:create-table (schema-table schema)
                                                       (schema-fields/nonvirtual schema)))))

(define/contract (drop-table! conn schema-or-name)
  (-> connection? (or/c schema? symbol?) void?)
  (define schema (schema-registry-lookup schema-or-name))
  (query-exec conn (dialect-emit-ddl (connection-dialect conn)
                                     (ast:drop-table (schema-table schema)))))


;; insert ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 insert!
 insert-one!)

(define/contract (insert! conn . entities)
  (-> connection? entity? ... (listof entity?))
  (define dialect (connection-dialect conn))
  (for/list ([entity (in-list entities)] #:when (meta-can-persist? (entity-meta entity)))
    (insert-entity! dialect conn entity)))

(define/contract (insert-one! conn entity)
  (-> connection? entity? (or/c false/c entity?))
  (match (insert! conn entity)
    [(list entity) entity]
    [_ #f]))

(define (insert-entity! dialect conn entity)
  (define meta (entity-meta entity))
  (define schema (meta-schema meta))
  (when (schema-virtual? schema)
    (raise-user-error 'insert-entity! "cannot insert entity ~v because it has a virtual schema" entity))

  (let ([entity ((schema-pre-persist-hook schema) entity)])
    (define-values (columns column-values)
      (for*/fold ([columns null]
                  [column-values null])
                 ([f (in-list (schema-fields schema))]
                  #:unless (or (field-auto-increment? f)
                               (field-virtual? f)))
        (values (cons (field-name f) columns)
                (cons (type-dump/null (field-type f)
                                      (dialect-name dialect)
                                      ((field-getter f) entity))
                      column-values))))

    (define pk
      (let ([pk (schema-primary-key schema)])
        (and pk (field-auto-increment? pk) pk)))
    (define stmt
      (ast:make-insert
       #:into (ast:table (schema-table schema))
       #:columns (map ast:column columns)
       #:values (map ast:placeholder column-values)
       #:returning (and pk (ast:returning (list (ast:column (field-name pk)))))))

    (define-values (query args)
      (dialect-emit-query dialect stmt))

    (let ([e ((schema-meta-updater schema) entity meta-track-persisted)])
      (cond
        [pk
         (define id
           (if (dialect-supports-returning? dialect)
               (apply query-value conn query args)
               (call-with-transaction conn
                 (lambda _
                   (apply query-exec conn query args)
                   (query-value conn (dialect-last-id-query dialect))))))

         ((field-setter pk) e id #f)]

        [else
         (begin0 e
           (apply query-exec conn query args))]))))


;; update ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 update!
 update-one!)

(define/contract (update! conn #:force? [force? #f] . entities)
  (->* (connection?)
       (#:force? boolean?)
       #:rest (listof entity?)
       (listof entity?))
  (define dialect (connection-dialect conn))
  (define maybe-updated
    (for/list ([entity (in-list entities)])
      (update-entity! dialect conn entity force?)))
  (filter values maybe-updated))

(define/contract (update-one! conn entity #:force? [force? #f])
  (->* (connection? entity?)
       (#:force? boolean?)
       (or/c false/c entity?))
  (match (update! conn entity #:force? force?)
    [(list e) e]
    [_ #f]))

(define (update-entity! dialect conn entity force?)
  (define schema (meta-schema (entity-meta entity)))
  (define pk (schema-primary-key schema))
  (unless pk
    (raise-argument-error 'update-entity! "entity with primary key field" entity))

  (cond
    [(or force? (meta-can-update? (entity-meta entity)))
     (define entity* ((schema-pre-persist-hook schema) entity))
     (define changes (meta-changes (entity-meta entity*)))
     (define-values (columns column-values)
       (for*/fold ([columns null]
                   [column-values null])
                  ([f (in-list (schema-fields schema))]
                   #:when (or force? (set-member? changes (field-id f)))
                   #:unless (or (field-auto-increment? f) (field-virtual? f)))
         (values (cons (field-name f) columns)
                 (cons (type-dump/null (field-type f)
                                       (dialect-name dialect)
                                       ((field-getter f) entity*))
                       column-values))))
     (cond
       [(null? columns) #f]
       [else
        (define stmt
          (ast:make-update
           #:table (ast:table (schema-table schema))
           #:assignments (ast:assignments
                          (for/list ([column (in-list columns)]
                                     [value (in-list column-values)])
                            (cons (ast:column column)
                                  (ast:placeholder value))))
           #:where (ast:where (ast:app (ast:ident '=)
                                       (list (ast:column (field-name pk))
                                             (ast:placeholder (type-dump/null (field-type pk)
                                                                              (dialect-name dialect)
                                                                              ((field-getter pk) entity*))))))))

        (define-values (query args)
          (dialect-emit-query dialect stmt))

        (apply query-exec conn query args)
        ((schema-meta-updater schema) entity* meta-track-persisted)])]

    [else #f]))


;; delete ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 delete!
 delete-one!)

(define/contract (delete! conn . entities)
  (-> connection? entity? ... (listof entity?))
  (define dialect (connection-dialect conn))
  (for/list ([entity (in-list entities)] #:when (meta-can-delete? (entity-meta entity)))
    (delete-entity! dialect conn entity)))

(define/contract (delete-one! conn entity)
  (-> connection? entity? (or/c false/c entity?))
  (match (delete! conn entity)
    [(list e) e]
    [_ #f]))

(define (delete-entity! dialect conn entity)
  (define meta (entity-meta entity))
  (define schema (meta-schema meta))
  (define pk (schema-primary-key schema))
  (unless pk
    (raise-argument-error 'delete-entity! "entity with primary key field" entity))

  (let ([entity ((schema-pre-delete-hook schema) entity)])
    (define stmt
      (ast:make-delete
       #:from (ast:make-from #:tables (list (ast:table (schema-table schema))))
       #:where (ast:where (ast:app (ast:ident '=)
                                   (list (ast:column (field-name pk))
                                         (ast:placeholder (type-dump/null (field-type pk)
                                                                          (dialect-name dialect)
                                                                          ((field-getter pk) entity))))))))

    (define-values (query args)
      (dialect-emit-query dialect stmt))

    (apply query-exec conn query args)
    ((schema-meta-updater schema) entity meta-track-deleted)))


;; select ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 in-entities
 lookup

 from
 group-by
 join
 limit
 offset
 or-where
 order-by
 returning
 select
 select-for-schema
 update
 where

 (rename-out
  [dyn:query? query?]
  [dyn:delete delete]
  [dyn:union union]
  [dyn:project-onto project-onto]
  [dyn:project-virtual-fields project-virtual-fields])

 (contract-out
  [make-entity
   (->* ((or/c connection? symbol?) schema? (listof any/c))
        (#:project-virtual-fields? boolean?)
        entity?)]))

(define (make-entity conn-or-dialect schema cols
                     #:project-virtual-fields? [project-virtual? #f])
  (define dialect
    (if (connection? conn-or-dialect)
        (dbsystem-name (connection-dbsystem conn-or-dialect))
        conn-or-dialect))
  (define pairs
    (for/fold ([pairs null])
              ([f (in-list (if project-virtual?
                               (schema-fields schema)
                               (schema-fields/nonvirtual schema)))]
               [v (in-list cols)])
      (cons (cons (field-kwd f)
                  (type-load/null (field-type f) dialect v))
            pairs)))

  (define pairs/sorted
    (sort pairs keyword<? #:key car))

  (define-values (kwds kw-args)
    (for/lists (kwds kw-args)
               ([pair (in-list pairs/sorted)])
      (values (car pair)
              (cdr pair))))

  (define e
    (keyword-apply (schema-struct-ctor schema) kwds kw-args null))

  ((schema-meta-updater schema) e meta-track-persisted))

(define-syntax-rule (define-keywords id ...)
  (begin
    (provide id ...)
    (define-syntax (id stx)
      (raise-syntax-error 'id "not allowed outside of a query" stx)) ...))

(define-keywords
  array as subquery fragment)

(define/contract (in-entities conn q
                              #:batch-size [batch-size +inf.0])
  (->* (connection? dyn:query?)
       (#:batch-size (or/c exact-positive-integer? +inf.0))
       sequence?)
  (define dialect
    (dbsystem-name (connection-dbsystem conn)))
  (define results-seq
    (in-query conn q #:fetch batch-size))
  (cond
    [(dyn:query-schema q)
     => (lambda (s)
          (define project-virtual?
            (dyn:opts-project-virtual-fields? (dyn:query-opts q)))

          (sequence-map
           (lambda cols
             (make-entity dialect s cols
                          #:project-virtual-fields? project-virtual?))
           results-seq))]

    [else
     results-seq]))

(define/contract (lookup conn q)
  (-> connection? dyn:query? any)
  (define-values (res _)
    (sequence-generate* (in-entities conn q)))

  (and res (apply values res)))

(begin-for-syntax
  (define column-reference-re
    #rx"^([^\\.]+)\\.([^\\.]+)$")

  (define (column-reference? id)
    (regexp-match? column-reference-re (symbol->string id)))

  (define (syntax->column-reference stx)
    (match-define (list _ a b)
      (regexp-match column-reference-re (symbol->string (syntax->datum stx))))

    (cons (datum->syntax stx a)
          (datum->syntax stx (id->column-name b))))

  (define-syntax-class placeholder-expr
    #:literals (unquote)
    (pattern (unquote placeholder) #:with e #'(ast:placeholder placeholder)))

  (define-syntax-class fragment-expr
    #:literals (fragment)
    (pattern (fragment node:expr) #:with e #'node))

  (define-syntax-class subquery-expr
    #:literals (subquery)
    (pattern (subquery q:expr) #:with e #'(dyn:subquery q)))

  (define-syntax-class q-source
    #:literals (unquote)
    (pattern schema:id #:with e #''schema)
    (pattern table:str #:with e #'table)
    (pattern sub:subquery-expr #:with e #'sub.e)
    (pattern (unquote e:expr)))

  (define-syntax-class q-expr
    #:datum-literals (list null)
    #:literals (array as and case cond else or quote subquery unquote-splicing)
    (pattern column-reference:id
             #:when (column-reference? (syntax->datum this-syntax))
             #:with e (let ([ref (syntax->column-reference this-syntax)])
                        #`(ast:qualified #,(car ref) #,(cdr ref))))

    (pattern placeholder:placeholder-expr
             #:with e #'placeholder.e)

    (pattern fragment:fragment-expr
             #:with e #'fragment.e)

    (pattern ident:id
             #:with e #'(ast:ident 'ident))

    (pattern b:boolean
             #:with e #'(ast:scalar b))

    (pattern s:string
             #:with e #'(ast:scalar s))

    (pattern n:number
             #:when (rational? (syntax->datum #'n))
             #:with e #'(ast:scalar n))

    (pattern (array item:q-expr ...)
             #:with e #'(ast:scalar (vector item.e ...)))

    (pattern (as a:q-expr b:id)
             #:with e #'(ast:as a.e 'b))

    (pattern (and a:q-expr b:q-expr)
             #:with e #'(ast:app (ast:ident 'and) (list a.e b.e)))

    (pattern ((~or case cond) [c:q-expr v:q-expr] ...+
                              [else ve:q-expr])
             #:with e #'(ast:case-e (list (cons c.e v.e) ...) ve.e))

    (pattern ((~or case cond) [c:q-expr v:q-expr] ...+)
             #:with e #'(ast:case-e (list (cons c.e v.e) ...) #f))

    (pattern (list item:q-expr ...)
             #:with e #'(ast:scalar (list item.e ...)))

    (pattern (quote (item:q-expr ...))
             #:with e #'(ast:scalar (list item.e ...)))

    (pattern (unquote-splicing items:expr)
             #:with e #'(ast:scalar (map ast:scalar items)))

    (pattern (or a:q-expr b:q-expr)
             #:with e #'(ast:app (ast:ident 'or) (list a.e b.e)))

    (pattern (subquery q)
             #:with e #'(ast:subquery (dyn:query-stmt q)))

    (pattern (fun:q-expr arg:q-expr ...)
             #:with e #'(ast:app fun.e (list arg.e ...))))

  (define-syntax-class q-assignment
    (pattern [column:id value:q-expr]
             #:fail-when (string-contains? (symbol->string (syntax->datum #'column)) ".") "assignments may not be qualified"
             #:with e (with-syntax ([name (datum->syntax #'r (id->column-name (syntax->datum #'column)))])
                        #'(cons (ast:column name) value.e))))

  (define-syntax-class q-ordering
    #:literals (unquote)
    (pattern [column:q-expr (~optional
                             (~seq (~or (~and #:asc  dir-asc )
                                        (~and #:desc dir-desc)
                                        (unquote dir-expr:expr))
                                   (~optional
                                    (~or (~and #:nulls-first nulls-first*)
                                         (~and #:nulls-last  nulls-last* )
                                         (unquote nulls-expr:expr)))))]
             #:with dir
             (cond [(attribute dir-expr) #'dir-expr]
                   [(attribute dir-desc) #''desc]
                   [else #''asc])
             #:with nulls-dir
             (cond [(attribute nulls-expr) #'nulls-expr]
                   [(attribute nulls-first*) #''nulls-first]
                   [(attribute nulls-last*) #''nulls-last]
                   [else #'#f])
             #:with e #'(list column.e dir nulls-dir))))

(define-syntax (from stx)
  (syntax-parse stx
    [(_ source:q-source #:as alias:id)
     #'(dyn:from source.e #:as 'alias)]))

(define-syntax (group-by stx)
  (syntax-parse stx
    [(_ q:expr e:q-expr ...+)
     #'(dyn:group-by q e.e ...)]))

(define-syntax (join stx)
  (define-syntax-class join-type
    (pattern #:inner #:with type #''inner)
    (pattern #:left  #:with type #''left)
    (pattern #:right #:with type #''right)
    (pattern #:full  #:with type #''full)
    (pattern #:cross #:with type #''cross))

  (syntax-parse stx
    [(_ q:expr
        (~optional t:join-type)
        (~optional (~and #:lateral lateral))
        source:q-source
        #:as alias:id
        #:on constraint:q-expr)
     #:with type #'(~? t.type 'inner)
     #:with lateral? (if (attribute lateral) #'#t #'#f)
     #'(dyn:join q
                 #:type type
                 #:lateral? lateral?
                 #:with source.e
                 #:as 'alias
                 #:on constraint.e)]))

(define-syntax (limit stx)
  (syntax-parse stx
    [(_ q:expr n:number)
     #:fail-when (and (not (exact-nonnegative-integer? (syntax->datum #'n))) #'n) "n must be a positive integer"
     #'(dyn:limit q (ast:scalar n))]

    [(_ q:expr p:placeholder-expr)
     #'(dyn:limit q p.e)]))

(define-syntax (offset stx)
  (syntax-parse stx
    [(_ q:expr n:number)
     #:fail-when (and (not (exact-nonnegative-integer? (syntax->datum #'n))) #'n) "n must be a positive integer"
     #'(dyn:offset q (ast:scalar n))]

    [(_ q:expr p:placeholder-expr)
     #'(dyn:offset q p.e)]))

(define-syntax (or-where stx)
  (syntax-parse stx
    [(_ q:expr e:q-expr)
     #'(dyn:or-where q e.e)]))

(define-syntax (order-by stx)
  (syntax-parse stx
    [(_ q:expr (e:q-ordering ...+))
     #'(dyn:order-by q e.e ...)]))

(define-syntax (returning stx)
  (syntax-parse stx
    [(_ q:expr e:q-expr ...+)
     #'(dyn:returning q e.e ...)]))

(define-syntax (select stx)
  (define-syntax-class from-expr
    #:datum-literals (_)
    (pattern _ #:with e #'(dyn:make-empty-query))
    (pattern e:expr))

  (syntax-parse stx
    [(_ q:from-expr
        (~alt (~optional (~and #:distinct distinct))) ...
        e:q-expr ...+)
     #:with distinct? (if (attribute distinct) #'#t #'#f)
     #'(dyn:select
        #:distinct? distinct?
        q.e e.e ...)]))

(define-syntax (select-for-schema stx)
  (define-syntax-class schema-expr
    #:literals (unquote)
    (pattern schema:id #:with e #''schema)
    (pattern (unquote e:expr)))

  (syntax-parse stx
    [(_ q:expr schema:schema-expr
        #:from tbl-alias:id)
     #'(select-for-schema q schema #:from tbl-alias #:customizing ())]

    [(_ q:expr schema:schema-expr
        #:from tbl-alias:id
        #:customizing ([fld-id:id e:q-expr] ...))
     #'(dyn:select-for-schema q schema.e
                              (symbol->string 'tbl-alias)
                              (make-hasheq (list (cons 'fld-id e.e) ...)))]))

(define-syntax (update stx)
  (syntax-parse stx
    [(_ q:expr ass:q-assignment ...+)
     #'(dyn:update q ass.e ...)]))

(define-syntax (where stx)
  (syntax-parse stx
    [(_ q:expr e:q-expr)
     #'(dyn:where q e.e)]))
