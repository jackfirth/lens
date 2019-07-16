#lang info

(define collection 'multi)

(define deps
  '(("racket" #:version "6.3")
    "base"
    "lens-common"
    "rackunit-lib"
    "fancy-app"
    "syntax-classes-lib"
    "struct-update-lib"
    "kw-make-struct"
    "reprovide-lang-lib"
    ))

(define build-deps
  '("sweet-exp-lib"
    ))

(define cover-omit-paths
  '(#rx"info\\.rkt"
    #rx"main\\.rkt"
    ))

