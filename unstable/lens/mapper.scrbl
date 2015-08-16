#lang scribble/manual

@(require lens/doc-util/main)

@title{Lenses that map over lists}

@defmodule[unstable/lens/mapper]

@defproc[(mapper-lens [lens lens?]) lens?]{
Creates a lens that maps @racket[lens] over a target list.
@lenses-unstable-examples[
  (lens-view (mapper-lens first-lens) '((a b) (c d) (e f)))
  (lens-set (mapper-lens first-lens) '((a b) (c d) (e f)) '(1 2 3))
  (lens-transform (mapper-lens first-lens) '((a b) (c d) (e f)) (λ (xs) (map symbol->string xs)))
]}
