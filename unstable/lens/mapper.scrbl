#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Lenses that map over lists and vectors}

@defmodule[unstable/lens/mapper]

@defproc[(mapper-lens [lens lens?]) lens?]{
Creates a lens that maps @racket[lens] over a target list.
@lenses-unstable-examples[
  (lens-view (mapper-lens first-lens) '((a b) (c d) (e f)))
  (lens-set (mapper-lens first-lens) '((a b) (c d) (e f)) '(1 2 3))
  (lens-transform (mapper-lens first-lens) '((a b) (c d) (e f)) (λ (xs) (map symbol->string xs)))
]}

@defproc[(vector-mapper-lens [lens lens?]) lens?]{
Creates a lens that maps @racket[lens] over a target vector with @racket[vector-map].
@lenses-unstable-examples[
  (lens-view (vector-mapper-lens first-lens) '#((a b) (c d) (e f)))
  (lens-set (vector-mapper-lens first-lens) '#((a b) (c d) (e f)) '#(1 2 3))
  (lens-transform (vector-mapper-lens first-lens) '#((a b) (c d) (e f))
                  (λ (xs) (vector->immutable-vector (vector-map symbol->string xs))))
]}
