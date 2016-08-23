#lang info

(define collection 'multi)

(define deps
  '("base"
    "lens-lib"
    "reprovide-lang"
    ))

(define build-deps
  '("sweet-exp-lib"
    ))

(define cover-omit-paths
  '(#rx"info\\.rkt"
    #rx"main\\.rkt"
    "unstable/lens/struct-provide.rkt"
    "unstable/lens/syntax.rkt"
    "unstable/lens/zoom.rkt"
    ))

