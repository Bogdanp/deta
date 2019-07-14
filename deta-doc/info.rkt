#lang info

(define collection "deta")
(define scribblings '(("deta.scrbl")))

(define deps '("base"))
(define build-deps '("db-lib"
                     "deta-lib"
                     "gregor-lib"
                     "sandbox-lib"
                     "scribble-lib"
                     "sql"
                     "threading-lib"

                     "db-doc"
                     "gregor-doc"
                     "racket-doc"
                     "threading-doc"))

(define update-implies '("deta-lib"))
