#lang info

(define collection 'multi)

(define deps
  '("base"
    "lens-lib"
    "lens-unstable"
    "scribble-lib"
    "reprovide-lang-lib"
    "jack-scribble-example"
    ))

(define build-deps
  '("at-exp-lib"
    "doc-coverage"
    "racket-doc"
    "sweet-exp-lib"
    ))

(define cover-omit-paths
  '(#rx".*\\.scrbl"
    #rx"info\\.rkt"
    "lens/private/doc-util"
    ))

