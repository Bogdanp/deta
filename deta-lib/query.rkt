#lang racket/base

(require db
         racket/contract
         racket/match
         racket/sequence
         "adapter/adapter.rkt"
         "adapter/postgresql.rkt"
         "adapter/sqlite3.rkt"
         "adapter/ast.rkt"
         "schema.rkt"
         "private/field.rkt"
         "private/meta.rkt"
         "private/type.rkt")

(define schema-or-name/c
  (or/c schema? symbol?))


;; ddl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 create-table!)

(define/contract (create-table! conn schema-or-name)
  (-> connection? schema-or-name/c void?)
  (query-exec conn (adapter-emit-ddl (connection-adapter conn)
                                     (lookup-schema schema-or-name))))


;; insert ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 insert!)

(define/contract (insert! conn . entities)
  (-> connection? entity? ... (listof entity?))
  (define adapter (connection-adapter conn))
  (for/list ([entity (in-list entities)] #:when (meta-can-persist? (entity-meta entity)))
    (insert-entity! adapter conn entity)))

(define (insert-entity! adapter conn entity)
  (define meta (entity-meta entity))
  (define schema (meta-schema meta))
  (define-values (columns getters)
    (for*/fold ([columns null]
                [getters null])
               ([f (in-list (schema-fields schema))]
                [p (in-value (type-dump (field-type f) f))]
                #:unless (field-auto-increment? f))
      (values (append (map car p) columns)
              (append (map cdr p) getters))))

  (define pk (schema-primary-key schema))
  (define pk-column
    (and pk (column-expr (field-name pk))))

  (define stmt
    (insert-stmt (table-expr (schema-table-name schema)) columns pk-column))

  (define query
    (adapter-emit-query adapter stmt))

  (define args
    (for/list ([getter (in-list getters)])
      (getter entity)))

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


;; select ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-entity schema cols)
  (define pairs
    (for/fold ([pairs null])
              ([f (in-list (schema-fields schema))]
               [v (in-list cols)])
      (append (type-load (field-type f) f v) pairs)))

  (define pairs/sorted
    (sort pairs keyword<? #:key car))

  (define-values (kwds kw-args)
    (for/lists (kwds kw-args)
               ([pair (in-list pairs/sorted)])
      (values (car pair)
              (cdr pair))))

  (keyword-apply (schema-struct-ctor schema) kwds kw-args null))

(define/contract (in-rows conn stmt . args)
  (-> connection? stmt? any/c ... sequence?)
  (define adapter (connection-adapter conn))
  (define schema
    (stmt-schema stmt))

  (sequence-map (lambda cols
                  (make-entity schema cols))
                (apply in-query conn (adapter-emit-query adapter stmt) args)))

(define/contract (in-row conn stmt . args)
  (-> connection? stmt? any/c ... sequence?)
  (let ([consumed #f])
    (stop-before (apply in-rows conn stmt args) (lambda _
                                                  (begin0 consumed
                                                    (set! consumed #t))))))

(define/contract (from schema-or-name #:as [alias #f])
  (->* (schema-or-name/c)
       (#:as (or/c false/c symbol?))
       from-clause?)

  (define schema
    (lookup-schema schema-or-name))

  (from-clause schema (if alias
                          (alias-expr (table-expr (schema-table-name schema)) alias)
                          (table-expr (schema-table-name schema)))))

(define/contract (select q)
  (-> (or/c from-clause? stmt?) select-stmt?)

  (define schema
    (from-clause-schema q))

  (define column:exprs
    (for/list ([f (in-list (schema-fields schema))])
      (column-expr (field-name f))))

  (select-stmt q column:exprs #f))


;; common ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (connection-adapter conn)
  (match (dbsystem-name (connection-dbsystem conn))
    ['postgresql postgresql-adapter]
    ['sqlite3    sqlite3-adapter]
    [_           (error 'connection-adapter "this connection type is not supported")]))

(define (lookup-schema schema-or-name)
  (define schema
    (match schema-or-name
      [(? schema?)                      schema-or-name ]
      [(? symbol?) (schema-registry-ref schema-or-name)]))

  (unless schema
    (raise-argument-error 'lookup-schema "unregistered schema" schema-or-name))

  schema)
