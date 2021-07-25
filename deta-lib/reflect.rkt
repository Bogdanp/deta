#lang racket/base

(require "private/field.rkt"
         "private/schema.rkt"
         "private/type.rkt")

(provide
 current-schema-registry
 schema-registry-allow-conflicts?
 schema-registry-lookup

 schema-virtual?
 schema-fields
 schema-table

 field?
 field-id
 field-name
 field-type

 type?
 type-declaration)
