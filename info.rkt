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
  '("cover"
    "cover-coveralls"
    "rackunit-lib"
    "racket-doc"
    "jack-scribble-example"
    "doc-coverage"))


(define test-omit-paths
  '())
