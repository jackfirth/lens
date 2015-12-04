#lang info

(define collection 'multi)


(define version "2.0")


(define deps
  '("jack-scribble-example"
    "base"
    "rackunit-lib"
    "unstable-lib"
    "unstable-list-lib"
    "unstable-contract-lib"
    "fancy-app"
    "alexis-util"
    "sweet-exp"
    "kw-make-struct"
    "reprovide-lang"
    "scribble-lib"))


(define build-deps
  '("at-exp-lib"
    "rackunit-lib"
    "racket-doc"
    "jack-scribble-example"
    "doc-coverage"))

(define cover-omit-paths
  '(#rx".*\\.scrbl"
    "info.rkt"
    "lens/private/doc-util"
    "lens/private/test-util"
    "lens/private/util"
    "lens/private"
    "lens/main.rkt"
    "unstable/lens/struct-provide.rkt"))
