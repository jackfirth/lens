#lang info

(define collection 'multi)


(define version "2.0")


(define deps
  '(("base" #:version "6.2.900.15")
    "rackunit-lib"
    "unstable-lib"
    "fancy-app"
    "alexis-util"
    "sweet-exp"
    "kw-make-struct"
    "reprovide-lang"
    "scribble-lib"))


(define build-deps
  '("cover"
    "rackunit-lib"
    "racket-doc"
    "jack-scribble-example"
    "doc-coverage"))


(define test-omit-paths
  '("info.rkt"
    "lens/private/base/base.scrbl"
    "lens/private/base/contract.scrbl"
    "lens/private/base/laws.scrbl"
    "lens/private/base/main.scrbl"
    "lens/private/base/transform.scrbl"
    "lens/private/base/view-set.scrbl"
    "lens/private/compound/compose.scrbl"
    "lens/private/compound/join-hash.scrbl"
    "lens/private/compound/join-list.scrbl"
    "lens/private/compound/join-string.scrbl"
    "lens/private/compound/join-vector.scrbl"
    "lens/private/compound/main.scrbl"
    "lens/private/compound/thrush.scrbl"
    "lens/private/doc-util"
    "lens/private/hash/main.scrbl"
    "lens/private/hash/nested.scrbl"
    "lens/private/hash/pick.scrbl"
    "lens/private/hash/ref.scrbl"
    "lens/private/list/assoc.scrbl"
    "lens/private/list/car-cdr.scrbl"
    "lens/private/list/list-ref-take-drop.scrbl"
    "lens/private/list/main.scrbl"
    "lens/private/list/multi.scrbl"
    "lens/private/struct/field.scrbl"
    "lens/private/struct/main.scrbl"
    "lens/private/struct/struct.scrbl"
    "lens/private/test-util"
    "lens/private/vector/main.scrbl"
    "lens/private/vector/nested.scrbl"
    "lens/private/vector/pick.scrbl"
    "lens/private/vector/ref.scrbl"
    "lens/applicable.scrbl"
    "lens/private/dict.scrbl"
    "lens/info.rkt"
    "lens/main.scrbl"
    "lens/private/stream.scrbl"
    "lens/private/string.scrbl"
    "unstable/lens/arrow.scrbl"
    "unstable/lens/main.scrbl"
    "unstable/lens/sublist.scrbl"
    "unstable/lens/syntax.scrbl"
    "unstable/lens/view-set.scrbl"))
