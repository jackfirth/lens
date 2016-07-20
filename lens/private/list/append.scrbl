#lang scribble/manual

@(require "../doc-util/main.rkt")

@title{Flattening and unflattening lists}

@defthing[append*-lens lens?]{
A lens that flattens a list one-level down when viewing, and restores
the original structure when setting.
@lens-unstable-examples[
  (lens-view append*-lens '((a) (b c) () (d e f)))
  (lens-set append*-lens '((a) (b c) () (d e f)) '(1 2 3 4 5 6))
]}

@defproc[(append*n-lens [n exact-nonnegative-integer?]) lens?]{
Creates a lens that flattens a list @racket[n] levels down when
viewing, and restores the original structure when setting.
@lens-unstable-examples[
  (lens-view (append*n-lens 2) '(((a) ()) (() (b) (c)) () ((d e) () (f))))
  (lens-set (append*n-lens 2) '(((a) ()) (() (b) (c)) () ((d e) () (f))) '(1 2 3 4 5 6))
]}
