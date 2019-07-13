#lang info

(define collection "tests")

(define deps '())
(define build-deps '("base"
                     "db-lib"
                     "deta-lib"
                     "gregor-lib"
                     "rackunit-lib"
                     "threading-lib"))

(define update-implies '("deta-lib"))
