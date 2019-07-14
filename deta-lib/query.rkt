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
         "adapter/adapter.rkt"
         "adapter/connection.rkt"
         "adapter/postgresql.rkt"
         "adapter/sqlite3.rkt"
         (prefix-in ast: "private/ast.rkt")
         "private/field.rkt"
         "private/meta.rkt"
         "private/type.rkt"
         (prefix-in dyn: "query/dynamic.rkt")
         "query/struct.rkt"
         "schema.rkt")

;; ddl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 create-table!
 drop-table!)

(define/contract (create-table! conn schema-or-name)
  (-> connection? (or/c schema? symbol?) void?)
  (define schema (schema-registry-lookup schema-or-name))
  (query-exec conn (adapter-emit-ddl (connection-adapter conn)
                                     (ast:create-table-ddl (schema-table-name schema)
                                                           (schema-fields schema)))))

(define/contract (drop-table! conn schema-or-name)
  (-> connection? (or/c schema? symbol?) void?)
  (define schema (schema-registry-lookup schema-or-name))
  (query-exec conn (adapter-emit-ddl (connection-adapter conn)
                                     (ast:drop-table-ddl (schema-table-name schema)))))


;; insert ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 insert!
 insert-one!)

(define/contract (insert! conn . entities)
  (-> connection? entity? ... (listof entity?))
  (define dialect (dbsystem-name (connection-dbsystem conn)))
  (define adapter (connection-adapter conn))
  (for/list ([entity (in-list entities)] #:when (meta-can-persist? (entity-meta entity)))
    (insert-entity! adapter dialect conn entity)))

(define/contract (insert-one! conn entity)
  (-> connection? entity? (or/c false/c entity?))
  (match (insert! conn entity)
    [(list entity) entity]
    [_ #f]))

(define (insert-entity! adapter dialect conn entity)
  (define meta (entity-meta entity))
  (define schema (meta-schema meta))
  (when (schema-virtual? schema)
    (raise-user-error 'insert-entity! "cannot insert entity ~v because it has a virtual schema" entity))

  (define-values (columns getters)
    (for*/fold ([columns null]
                [getters null])
               ([f (in-list (schema-fields schema))]
                #:unless (field-auto-increment? f))
      (define-values (col get)
        (type-dump (field-type f) f dialect))

      (values (cons col columns)
              (cons get getters))))

  (define pk (schema-primary-key schema))
  (define stmt
    (ast:insert (ast:table (schema-table-name schema))
                (map ast:column columns)
                (for/list ([getter (in-list getters)])
                  (ast:placeholder (getter entity)))
                (and pk (ast:column (field-name pk)))))

  (define-values (query args)
    (adapter-emit-query adapter stmt))

  (define res
    (cond
      [(adapter-supports-returning? adapter)
       (apply query-value conn query args)]

      [else
       (apply query-exec conn query args)
       (query-value conn (adapter-last-id-query adapter))]))

  (let ([e ((schema-meta-updater schema) entity meta-track-persisted)])
    (cond
      [(and res pk) ((field-setter pk) e res #f)]
      [else                            e        ])))


;; update ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 update!)

(define/contract (update! conn . entities)
  (-> connection? entity? ... (listof entity?))
  (define dialect (dbsystem-name (connection-dbsystem conn)))
  (define adapter (connection-adapter conn))
  (for/list ([entity (in-list entities)] #:when (meta-can-update? (entity-meta entity)))
    (update-entity! adapter dialect conn entity)))

(define (update-entity! adapter dialect conn entity)
  (define meta (entity-meta entity))
  (define schema (meta-schema meta))
  (define pk (schema-primary-key schema))
  (unless pk
    (raise-argument-error 'update-entity! "entity with primary key field" entity))

  (define changes (meta-changes meta))
  (define-values (columns getters)
    (for*/fold ([columns null]
                [getters null])
               ([f (in-list (schema-fields schema))]
                #:when (set-member? changes (field-id f))
                #:unless (field-auto-increment? f))
      (define-values (col get)
        (type-dump (field-type f) f dialect))

      (values (cons col columns)
              (cons get getters))))

  (define stmt
    (ast:update (ast:table (schema-table-name schema))
                (ast:assignments
                 (for/list ([column (in-list columns)]
                            [getter (in-list getters)])
                   (cons (ast:column column)
                         (ast:placeholder (getter entity)))))
                (ast:where (ast:app (ast:name '=)
                                    (list (ast:column (field-name pk))
                                          (ast:placeholder ((field-getter pk) entity)))))))

  (define-values (query args)
    (adapter-emit-query adapter stmt))

  (apply query-exec conn query args)
  ((schema-meta-updater schema) entity meta-track-persisted))


;; delete ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 delete!)

(define/contract (delete! conn . entities)
  (-> connection? entity? ... (listof entity?))
  (define adapter (connection-adapter conn))
  (for/list ([entity (in-list entities)] #:when (meta-can-delete? (entity-meta entity)))
    (delete-entity! adapter conn entity)))

(define (delete-entity! adapter conn entity)
  (define meta (entity-meta entity))
  (define schema (meta-schema meta))
  (define pk (schema-primary-key schema))
  (unless pk
    (raise-argument-error 'delete-entity! "entity with primary key field" entity))

  (define stmt
    (ast:delete (ast:from (ast:table (schema-table-name schema)))
                (ast:where (ast:app (ast:name '=)
                                    (list (ast:column (field-name pk))
                                          (ast:placeholder ((field-getter pk) entity)))))))

  (define-values (query args)
    (adapter-emit-query adapter stmt))

  (apply query-exec conn query args)
  ((schema-meta-updater schema) entity meta-track-deleted))


;; select ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 query?

 in-rows
 in-row
 lookup

 sql
 from
 group-by
 limit
 offset
 order-by
 select
 where
 and-where
 or-where

 (rename-out [dyn:project-onto project-onto]))

(define (make-entity-instance dialect schema cols)
  (define pairs
    (for/fold ([pairs null])
              ([f (in-list (schema-fields schema))]
               [v (in-list cols)])
      (define-values (kwd arg)
        (type-load (field-type f) f v dialect))

      (cons (cons kwd arg) pairs)))

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

(define/contract (in-rows conn q)
  (-> connection? query? sequence?)
  (define dialect (dbsystem-name (connection-dbsystem conn)))
  (define schema (query-schema q))
  (sequence-map (lambda cols
                  (if schema
                      (make-entity-instance dialect schema cols)
                      (apply values cols)))
                (in-query conn q)))

(define/contract (in-row conn q)
  (-> connection? query? sequence?)
  (let ([consumed #f])
    (stop-before (in-rows conn q) (lambda _
                                    (begin0 consumed
                                      (set! consumed #t))))))

(define/contract (lookup conn q)
  (-> connection? query? (or/c false/c entity?))
  (for/first ([e (in-row conn q)]) e))

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

  (define-syntax-class q-expr
    #:datum-literals (as list null)
    #:literals (and case else or quote unquote)
    (pattern ref:id
             #:when (column-reference? (syntax->datum this-syntax))
             #:with e (let ([ref (syntax->column-reference this-syntax)])
                        #`(ast:qualified #,(car ref) #,(cdr ref))))

    (pattern name:id
             #:with e #'(ast:name 'name))

    (pattern b:boolean
             #:with e #'(ast:scalar b))

    (pattern s:string
             #:with e #'(ast:scalar s))

    (pattern n:number
             #:when (rational? (syntax->datum #'n))
             #:with e #'(ast:scalar n))

    (pattern (as a:q-expr b:id)
             #:with e #'(ast:as a.e 'b))

    (pattern (and a:q-expr b:q-expr)
             #:with e #'(ast:app (ast:name 'and) (list a.e b.e)))

    (pattern (case [c:q-expr v:q-expr] ...+
                   [else ve:q-expr])
             #:with e #'(ast:case-e (list (cons c.e v.e) ...) ve.e))

    (pattern (case [c:q-expr v:q-expr] ...+)
             #:with e #'(ast:case-e (list (cons c.e v.e) ...) #f))

    (pattern (or a:q-expr b:q-expr)
             #:with e #'(ast:app (ast:name 'or) (list a.e b.e)))

    (pattern (~or (list v:q-expr ...)
                  (quote (v:q-expr ...)))
             #:with e #'(ast:scalar (list v.e ...)))

    (pattern (unquote v)
             #:with e #'(ast:placeholder v))

    (pattern (f:q-expr arg:q-expr ...)
             #:with e #'(ast:app f.e (list arg.e ...))))

  (define-syntax-class q-order-pair
    (pattern (c:q-expr (~or (~optional (~and #:asc dir-asc))
                            (~optional (~and #:desc dir-desc))))
             #:with dir (if (attribute dir-desc) #''desc #''asc)
             #:with e #'(cons c.e dir))))

(define-syntax (sql stx)
  (syntax-parse stx
    [(_ e:q-expr) #'e.e]))

(define-syntax (from stx)
  (syntax-parse stx
    [(_ schema:str #:as alias:id)
     #'(dyn:from schema #:as 'alias)]

    [(_ schema:id #:as alias:id)
     #'(dyn:from 'schema #:as 'alias)]))

(define-syntax (select stx)
  (syntax-parse stx
    [(_ e:q-expr ...+)
     #'(dyn:select (make-empty-query) e.e ...)]

    [(_ q:expr e:q-expr ...+)
     #'(dyn:select q e.e ...)]))

(define-syntax (limit stx)
  (syntax-parse stx
    [(_ q:expr n:number)
     #'(dyn:limit q n)]))

(define-syntax (group-by stx)
  (syntax-parse stx
    [(_ q:expr e:q-expr ...+)
     #'(dyn:group-by q e.e ...)]))

(define-syntax (offset stx)
  (syntax-parse stx
    [(_ q:expr n:number)
     #'(dyn:offset q n)]))

(define-syntax (order-by stx)
  (syntax-parse stx
    [(_ q:expr (e:q-order-pair ...+))
     #'(dyn:order-by q e.e ...)]))

(define-syntax (where stx)
  (syntax-parse stx
    [(_ q:expr e:q-expr)
     #'(dyn:where q e.e)]))

(define-syntax (and-where stx)
  (syntax-parse stx
    [(_ q:expr e:q-expr)
     #'(dyn:and-where q e.e)]))

(define-syntax (or-where stx)
  (syntax-parse stx
    [(_ q:expr e:q-expr)
     #'(dyn:or-where q e.e)]))
