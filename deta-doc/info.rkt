#lang info

(define license 'BSD-3-Clause)
(define collection "deta")
(define scribblings '(("deta.scrbl")))

(define deps '("base"))
(define build-deps '("db-doc"
                     "db-lib"
                     "deta-lib"
                     "gregor-doc"
                     "gregor-lib"
                     "racket-doc"
                     "sandbox-lib"
                     "scribble-lib"
                     "threading-doc"
                     "threading-lib"))

(define update-implies '("deta-lib"))
