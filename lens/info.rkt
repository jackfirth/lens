#lang info

(define collection 'multi)

(define deps
  '("base"
    "lens-common"
    "lens-data"
    "lens-lib"
    "lens-unstable"
    "lens-doc"
    ))

(define implies
  '("lens-common"
    "lens-data"
    "lens-lib"
    "lens-unstable"
    "lens-doc"
    ))

(define update-implies
  '("lens-common"
    "lens-data"
    "lens-lib"
    "lens-unstable"
    "lens-doc"
    ))

(define cover-omit-paths
  '(#rx"info\\.rkt"
    ))

