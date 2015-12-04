#lang scribble/manual

@(require "../doc-util/main.rkt")

@title{Lens Contracts}

@defproc[(lens/c [target/c contract?] [view/c contract?]) contract?]{
A contract constructor for lenses. The @racket[target/c] contract is used for
any target given to or returned by the lens, while the @racket[view/c] contract
is used for any view given to or returned by the lens. For example, the
@racket[view/c] contract is used for the return value of
@racket[(lens-view lens target)] and the third argument of
@racket[(lens-set lens target view)], as well as other places where targets or
views of the lens are used as inputs or outputs.
@lens-examples[
  (define contracted-car-lens
    (invariant-assertion (lens/c pair? number?) car-lens))
  (lens-view contracted-car-lens (cons 1 2))
  (lens-view contracted-car-lens 'not-a-pair)
  (lens-view contracted-car-lens (cons 'not-a-number 2))
  (lens-set contracted-car-lens (cons 1 2) 'not-a-number)
]}
