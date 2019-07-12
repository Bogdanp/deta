#lang info

(define collection "test")

(define deps '())
(define build-deps '("base"
                     "deta-lib"
                     "rackunit-lib"))

(define update-implies '("deta-lib"))
