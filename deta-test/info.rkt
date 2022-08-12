#lang info

(define collection "tests")

(define deps '())
(define build-deps
  '("base"
    "at-exp-lib"
    "db-lib"
    "deta-lib"
    "gregor-lib"
    ["libsqlite3-x86_64-linux" #:platform #rx"x86_64-linux(?!-natipkg)"]
    "rackunit-lib"
    "threading-lib"))

(define update-implies '("deta-lib"))
