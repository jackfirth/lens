#lang info

(define collection 'multi)

(define version "2.0")

(define deps
  '("base"
    "lens-common"
    "lens-data"
    "reprovide-lang"
    ))

(define implies
  '("lens-common"
    "lens-data"
    ))

(define update-implies
  '("lens-common"
    "lens-data"
    ))

(define build-deps
  '("sweet-exp-lib"
    ))

(define cover-omit-paths
  '(#rx"info\\.rkt"
    #rx"main\\.rkt"
    ))

