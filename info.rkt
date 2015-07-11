#lang info

(define collection 'multi)


(define version "1.2")


(define deps
  '("base"
    "rackunit-lib"
    "fancy-app"
    "alexis-util"
    "scribble-lib"))


(define build-deps
  '("cover"
    "rackunit-lib"
    "racket-doc"
    "doc-coverage"))


(define test-omit-paths
  '("info.rkt"
    "lens/info.rkt"
    "lens/applicable.scrbl"
    "lens/deflenses.rkt"
    "lens/lenses-examples.rkt"
    "lens/main.scrbl"
    "lens/syntax.scrbl"
    "lens/base/base.scrbl"
    "lens/base/compose.scrbl"
    "lens/base/main.scrbl"
    "lens/base/transform.scrbl"
    "lens/base/view-set.scrbl"
    "lens/list/assoc.scrbl"
    "lens/list/car-cdr.scrbl"
    "lens/list/list-ref-take-drop.scrbl"
    "lens/list/main.scrbl"
    "unstable/lens/main.scrbl"
    "unstable/lens/compound.scrbl"
    "unstable/lens/syntax.scrbl"))
