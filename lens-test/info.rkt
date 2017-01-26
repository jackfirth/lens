#lang info

(define collection 'multi)

(define deps '())

(define build-deps
  '("base"
    "rackunit-lib"
    "lens-lib"
    "sweet-exp-lib"
    ))

(define cover-omit-paths
  '(#rx"info\\.rkt"
    #rx"main\\.rkt"
    ))

