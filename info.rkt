#lang info

(define collection 'multi)


(define version "0.3")


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
    "lens/core/base.scrbl"
    "lens/core/compose.scrbl"
    "lens/core/main.scrbl"
    "lens/core/transform.scrbl"
    "lens/core/view-set.scrbl"
    "lens/list/assoc.scrbl"
    "lens/list/car-cdr.scrbl"
    "lens/list/list-ref-take-drop.scrbl"
    "lens/list/main.scrbl"))
