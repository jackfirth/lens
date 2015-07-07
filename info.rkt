#lang info

(define collection 'multi)


(define version 0.2)


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
    "lenses/info.rkt"
    "lenses/applicable.scrbl"
    "lenses/deflenses.rkt"
    "lenses/lenses-examples.rkt"
    "lenses/main.scrbl"
    "lenses/syntax.scrbl"
    "lenses/core/base.scrbl"
    "lenses/core/compose.scrbl"
    "lenses/core/main.scrbl"
    "lenses/core/transform.scrbl"
    "lenses/core/view-set.scrbl"
    "lenses/list/assoc.scrbl"
    "lenses/list/car-cdr.scrbl"
    "lenses/list/list-ref-take-drop.scrbl"
    "lenses/list/main.scrbl"))
