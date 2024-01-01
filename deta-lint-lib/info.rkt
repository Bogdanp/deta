#lang info

(define license 'BSD-3-Clause)
(define collection "deta")
(define deps
  '("base"
    "review"))
(define review-exts
  '((deta/review should-review-syntax? review-syntax)))
