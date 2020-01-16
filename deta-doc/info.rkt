#lang info

(define collection "deta")
(define scribblings '(("deta.scrbl")))

(define deps '("base"))
(define build-deps '("db-doc"
                     "db-lib"
                     "deta-lib"
                     "gregor-lib"
                     "racket-doc"
                     "sandbox-lib"
                     "scribble-lib"
                     "threading-doc"
                     "threading-lib"))

(define update-implies '("deta-lib"))
