#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         db
         racket/contract
         racket/match
         racket/sequence
         "adapter/adapter.rkt"
         "adapter/postgresql.rkt"
         "adapter/sqlite3.rkt"
         "ast.rkt"
         "schema.rkt"
         "private/field.rkt"
         "private/meta.rkt"
         "private/type.rkt"
         (prefix-in dyn: "query/dynamic.rkt"))

(define schema-or-name/c
  (or/c schema? symbol?))


;; ddl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 create-table!
 drop-table!)

(define/contract (create-table! conn schema-or-name)
  (-> connection? schema-or-name/c void?)
  (define schema (schema-registry-lookup schema-or-name))
  (query-exec conn (adapter-emit-ddl (connection-adapter conn)
                                     (create-table-ddl (schema-table-name schema)
                                                       (schema-fields schema)))))

(define/contract (drop-table! conn schema-or-name)
  (-> connection? schema-or-name/c void?)
  (define schema (schema-registry-lookup schema-or-name))
  (query-exec conn (adapter-emit-ddl (connection-adapter conn)
                                     (drop-table-ddl (schema-table-name schema)))))


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
    (insert-stmt (table-expr (schema-table-name schema))
                 (map column-expr columns)
                 (for/list ([i (in-range 1 (add1 (length columns)))])
                   (placeholder-expr i))
                 (and pk-column (column-expr pk-column))))

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
    (raise-argument-error 'delete-entity! "cannot delete entities without a primary key" entity))

  (define stmt
    (delete-stmt (from-clause schema (table-expr (schema-table-name schema)))
                 (where-clause (binary-expr '=
                                            (column-expr (field-name pk))
                                            (placeholder-expr 1)))))

  (query-exec conn (adapter-emit-query adapter stmt) ((field-getter pk) entity))
  ((schema-meta-updater schema) entity meta-track-deleted))


;; select ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 in-rows
 in-row

 from)

(define (make-entity-instance schema cols)
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
                  (make-entity-instance schema cols))
                (apply in-query conn (adapter-emit-query adapter stmt) args)))

(define/contract (in-row conn stmt . args)
  (-> connection? stmt? any/c ... sequence?)
  (let ([consumed #f])
    (stop-before (apply in-rows conn stmt args) (lambda _
                                                  (begin0 consumed
                                                    (set! consumed #t))))))

(define-syntax (from stx)
  (syntax-parse stx
    [(_ schema:id #:as alias:id)
     #'(dyn:from 'schema #:as 'alias)]))


;; common ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (connection-adapter conn)
  (match (dbsystem-name (connection-dbsystem conn))
    ['postgresql postgresql-adapter]
    ['sqlite3    sqlite3-adapter]
    [_           (error 'connection-adapter "this connection type is not supported")]))
