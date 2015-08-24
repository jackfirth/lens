#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Joining lenses with structs}

@defmodule[unstable/lens/struct-join]

@defform[(lens-join/struct struct-id field-lens ...)
         #:grammar ([field-lens (code:line lens-expr)
                                (code:line field-keyword lens-expr)])]{
Like @racket[lens-join/list], except that the views of the given
lenses are put in an instance of the @racket[struct-id] struct instead
of in a list. 
@lenses-unstable-examples[
  (struct foo (a b) #:transparent)
  (define lens (lens-join/struct foo first-lens third-lens))
  (lens-view lens '(1 2 3))
  (lens-set lens '(1 2 3) (foo 'a 'b))
]
Struct fields in a @racket[lens-join/struct] form can also be
specified by keywords, in any order, and even with some fields specied
by position and some by keywords:
@lenses-unstable-examples[
  (struct foo (a b) #:transparent)
  (lens-view (lens-join/struct foo first-lens third-lens) '(1 2 3))
  (lens-view (lens-join/struct foo #:a first-lens #:b third-lens) '(1 2 3))
  (lens-view (lens-join/struct foo #:b third-lens #:a first-lens) '(1 2 3))
  (lens-view (lens-join/struct foo first-lens #:b third-lens) '(1 2 3))
]}
