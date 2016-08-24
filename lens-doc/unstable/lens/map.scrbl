#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Lenses that map over lists and vectors}

@defproc[(map-lens [lens lens?]) lens?]{
Creates a lens that maps @racket[lens] over a target list.
@lens-unstable-examples[
  (lens-view (map-lens first-lens) '((a b) (c d) (e f)))
  (lens-set (map-lens first-lens) '((a b) (c d) (e f)) '(1 2 3))
  (lens-transform (map-lens first-lens) '((a b) (c d) (e f)) (λ (xs) (map symbol->string xs)))
]}

@defproc[(vector-map-lens [lens lens?]) lens?]{
Creates a lens that maps @racket[lens] over a target vector with @racket[vector-map].
@lens-unstable-examples[
  (lens-view (vector-map-lens first-lens) '#((a b) (c d) (e f)))
  (lens-set (vector-map-lens first-lens) '#((a b) (c d) (e f)) '#(1 2 3))
  (lens-transform (vector-map-lens first-lens) '#((a b) (c d) (e f))
                  (λ (xs) (vector->immutable-vector (vector-map symbol->string xs))))
]}
