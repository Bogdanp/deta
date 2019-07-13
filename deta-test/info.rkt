#lang info

(define collection "test")

(define deps '())
(define build-deps '("base"
                     "deta-lib"
                     "rackunit-lib"
                     "threading-lib"))

(define update-implies '("deta-lib"))
