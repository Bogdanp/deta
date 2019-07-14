#lang info

(define collection "deta")
(define scribblings '(("deta.scrbl")))

(define deps '("base"))
(define build-deps '("db-lib"
                     "deta-lib"
                     "gregor-lib"
                     "scribble-lib"))

(define update-implies '("deta-lib"))
