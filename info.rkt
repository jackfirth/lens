#lang info

(define collection 'multi)


(define version "2.0")


(define deps
  '("base"
    "rackunit-lib"
    "unstable-lib"
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
    "lens/base/base.scrbl"
    "lens/base/contract.scrbl"
    "lens/base/laws.scrbl"
    "lens/base/main.scrbl"
    "lens/base/transform.scrbl"
    "lens/base/view-set.scrbl"
    "lens/compound/compose.scrbl"
    "lens/compound/join-hash.scrbl"
    "lens/compound/join-list.scrbl"
    "lens/compound/join-string.scrbl"
    "lens/compound/join-vector.scrbl"
    "lens/compound/main.scrbl"
    "lens/compound/thrush.scrbl"
    "lens/doc-util"
    "lens/hash/main.scrbl"
    "lens/hash/nested.scrbl"
    "lens/hash/pick.scrbl"
    "lens/hash/ref.scrbl"
    "lens/list/assoc.scrbl"
    "lens/list/car-cdr.scrbl"
    "lens/list/list-ref-take-drop.scrbl"
    "lens/list/main.scrbl"
    "lens/list/multi.scrbl"
    "lens/struct/field.scrbl"
    "lens/struct/main.scrbl"
    "lens/struct/struct.scrbl"
    "lens/test-util"
    "lens/vector/main.scrbl"
    "lens/vector/nested.scrbl"
    "lens/vector/pick.scrbl"
    "lens/vector/ref.scrbl"
    "lens/applicable.scrbl"
    "lens/dict.scrbl"
    "lens/info.rkt"
    "lens/main.scrbl"
    "lens/stream.scrbl"
    "lens/string.scrbl"
    "unstable/lens/arrow.scrbl"
    "unstable/lens/main.scrbl"
    "unstable/lens/sublist.scrbl"
    "unstable/lens/syntax.scrbl"
    "unstable/lens/view-set.scrbl"))
