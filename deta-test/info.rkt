#lang info

(define collection "tests")

(define deps '(("libsqlite3-x86_64-linux" #:platform #rx"x86_64-linux")))
(define build-deps '("base"
                     "at-exp-lib"
                     "db-lib"
                     "deta-lib"
                     "gregor-lib"
                     "rackunit-lib"
                     "threading-lib"))

(define update-implies '("deta-lib"))
