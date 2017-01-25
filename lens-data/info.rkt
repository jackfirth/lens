#lang info

(define collection 'multi)

(define deps
  '("base"
    "lens-common"
    "rackunit-lib"
    "fancy-app"
    "syntax-classes-lib"
    "struct-update-lib"
    "kw-make-struct"
    "reprovide-lang"
    ))

(define build-deps
  '("sweet-exp-lib"
    ))

(define cover-omit-paths
  '(#rx"info\\.rkt"
    #rx"main\\.rkt"
    ))

