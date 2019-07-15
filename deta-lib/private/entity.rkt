#lang racket/base

(provide
 (struct-out entity))

(struct entity (meta)
  #:transparent)
