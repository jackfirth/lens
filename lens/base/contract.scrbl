#lang scribble/manual

@(require lens/doc-util/main)

@title{Lens Contracts}

@defproc[(lens/c [target/c contract?] [view/c contract?]) contract?]{
A contract constructor for lenses. The @racket[target/c] contract is used for
the second argument in @racket[(lens-view lens target)], the second argument
and the return value of @racket[(lens-set lens target view)], for example, the
@racket[view/c] contract is used for the return value of
@racket[(lens-view lens target)] and the third argument of
@racket[(lens-set lens target view)], as well as other places where targets or
views of the lens are used as inputs or outputs.
}

