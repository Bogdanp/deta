#lang racket/base

(require (for-syntax racket/base
                     racket/match
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
                                                       (schema-fields schema)))))

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
                  #:unless (field-auto-increment? f))
        (values (cons (field-name f) columns)
                (cons (type-dump (field-type f)
                                 (dialect-name dialect)
                                 ((field-getter f) entity))
                      column-values))))

    (define pk (schema-primary-key schema))
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

(define/contract (update! conn . entities)
  (-> connection? entity? ... (listof entity?))
  (define dialect (connection-dialect conn))
  (for/list ([entity (in-list entities)] #:when (meta-can-update? (entity-meta entity)))
    (update-entity! dialect conn entity)))

(define/contract (update-one! conn entity)
  (-> connection? entity? (or/c false/c entity?))
  (match (update! conn entity)
    [(list e) e]
    [_ #f]))

(define (update-entity! dialect conn entity)
  (define schema (meta-schema (entity-meta entity)))
  (define pk (schema-primary-key schema))
  (unless pk
    (raise-argument-error 'update-entity! "entity with primary key field" entity))

  (cond
    [(entity-changed? entity)
     (define entity* ((schema-pre-persist-hook schema) entity))
     (define changes (meta-changes (entity-meta entity*)))
     (define-values (columns column-values)
       (for*/fold ([columns null]
                   [column-values null])
                  ([f (in-list (schema-fields schema))]
                   #:when (set-member? changes (field-id f))
                   #:unless (field-auto-increment? f))
         (values (cons (field-name f) columns)
                 (cons (type-dump (field-type f)
                                  (dialect-name dialect)
                                  ((field-getter f) entity*))
                       column-values))))

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
                                          (ast:placeholder ((field-getter pk) entity*)))))))

     (define-values (query args)
       (dialect-emit-query dialect stmt))

     (apply query-exec conn query args)
     ((schema-meta-updater schema) entity* meta-track-persisted)]

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
                                         (ast:placeholder ((field-getter pk) entity)))))))

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
 update
 where

 (rename-out [dyn:query? query?])
 (rename-out [dyn:delete delete])
 (rename-out [dyn:project-onto project-onto]))

(define (make-entity-instance dialect schema cols)
  (define pairs
    (for/fold ([pairs null])
              ([f (in-list (schema-fields schema))]
               [v (in-list cols)])
      (cons (cons (field-kwd f)
                  (type-load (field-type f) dialect v))
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

(define/contract (in-entities conn q
                              #:batch-size [batch-size +inf.0])
  (->* (connection? dyn:query?)
       (#:batch-size (or/c exact-positive-integer? +inf.0))
       sequence?)
  (define dialect (dbsystem-name (connection-dbsystem conn)))
  (define schema (dyn:query-schema q))
  (sequence-map (lambda cols
                  (if schema
                      (make-entity-instance dialect schema cols)
                      (apply values cols)))
                (in-query conn q #:fetch batch-size)))

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
    (pattern (unquote placeholder)
             #:with e #'(ast:placeholder placeholder)))

  (define-syntax-class subquery-expr
    #:datum-literals (subquery)
    (pattern (subquery q:expr)
             #:with e #'(dyn:subquery q)))

  (define-syntax-class q-source
    (pattern schema:id #:with e #''schema)
    (pattern table:str #:with e #'table)
    (pattern sub:subquery-expr #:with e #'sub.e))

  (define-syntax-class q-expr
    #:datum-literals (array as list null)
    #:literals (and case cond else or)
    (pattern column-reference:id
             #:when (column-reference? (syntax->datum this-syntax))
             #:with e (let ([ref (syntax->column-reference this-syntax)])
                        #`(ast:qualified #,(car ref) #,(cdr ref))))

    (pattern placeholder:placeholder-expr
             #:with e #'placeholder.e)

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

    (pattern (or a:q-expr b:q-expr)
             #:with e #'(ast:app (ast:ident 'or) (list a.e b.e)))

    (pattern (fun:q-expr arg:q-expr ...)
             #:with e #'(ast:app fun.e (list arg.e ...))))

  (define-syntax-class q-assignment
    (pattern [column:id value:q-expr]
             #:with e (with-syntax ([name (datum->syntax #'r (id->column-name (syntax->datum #'column)))])
                        #'(cons (ast:column name) value.e))))

  (define-syntax-class q-order-pair
    (pattern [column:q-expr (~or (~optional (~and #:asc  dir-asc ))
                                 (~optional (~and #:desc dir-desc)))]
             #:with dir (if (attribute dir-desc) #''desc #''asc)
             #:with e #'(cons column.e dir))))

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
        #:with source:q-source
        #:as alias:id
        #:on constraint:q-expr)
     (with-syntax ([type #'(~? t.type 'inner)])
       #'(dyn:join q
                   #:type type
                   #:with source.e
                   #:as 'alias
                   #:on constraint.e))]))

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
    [(_ q:expr (e:q-order-pair ...+))
     #'(dyn:order-by q e.e ...)]))

(define-syntax (returning stx)
  (syntax-parse stx
    [(_ q:expr e:q-expr ...+)
     #'(dyn:returning q e.e ...)]))

(define-syntax (select stx)
  (syntax-parse stx
    #:datum-literals (_)
    [(select _ e:q-expr ...+)
     #'(dyn:select (dyn:make-empty-query) e.e ...)]

    [(select q:expr e:q-expr ...+)
     #'(dyn:select q e.e ...)]))

(define-syntax (update stx)
  (syntax-parse stx
    [(_ q:expr ass:q-assignment ...+)
     #'(dyn:update q ass.e ...)]))

(define-syntax (where stx)
  (syntax-parse stx
    [(_ q:expr e:q-expr)
     #'(dyn:where q e.e)]))
