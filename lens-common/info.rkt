#lang info

(define collection 'multi)

(define deps
  '(("racket" #:version "6.3")
    "base"
    "fancy-app"
    "rackunit-lib"
    "reprovide-lang-lib"
    ))

(define build-deps
  '("lens-data"
    "sweet-exp-lib"
    ))

(define cover-omit-paths
  '(#rx"info\\.rkt"
    #rx"main\\.rkt"
    "lens/common.rkt"
    "lens/private/test-util"
    "lens/private/util"
    ))

