#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Filtering sets}

@defmodule[unstable/lens/set-filterer]

@defproc[(set-filterer-lens [pred (-> any/c any/c)]) (lens/c functional-set? functional-set?)]{
Creates a lens that filters a set by the predicate @racket[pred].
@lenses-unstable-examples[
  (lens-view (set-filterer-lens number?) (set 1 'a 2 'b 'c 3 'd 'e))
  (lens-set (set-filterer-lens number?) (set 1 'a 2 'b 'c 3 'd 'e) (set 4 5 6 7))
]
Lists are also sets, so @racket[set-filterer-lens] works for lists too, but it
does not preserve ordering. It follows the lens laws only if you compare using
@racket[set=?], not @racket[equal?].
@lenses-unstable-examples[
  (lens-view (set-filterer-lens number?) '(a 1 2 3)) ; will be '(1 2 3)
  (lens-set (set-filterer-lens number?) '(a 1 2 3) '(1 2 3)) ; will be '(3 2 1 a)
  (code:comment "this breaks the lens laws according to equal?")
  (equal? '(a 1 2 3) '(3 2 1 a))
  (code:comment "but not according to set=?")
  (set=? '(a 1 2 3) '(3 2 1 a))
]}
