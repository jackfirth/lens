#lang scribble/manual

@(require "../doc-util/main.rkt")


@defform[(define-struct-lenses struct-id)]{
  Given a @racket[struct-id], defines a lens for each of its fields.
  @lens-examples[
    (struct foo (a b c) #:transparent)
    (define-struct-lenses foo)
    (lens-view foo-a-lens (foo 1 2 3))
    (lens-set foo-a-lens (foo 1 2 3) 100)
]}

@defform[(struct/lens struct-id (field-spec ...) struct-option ...)]{
  Equivalent to @racket[struct] and @racket[define-struct-lenses] combined.
  @lens-examples[
    (struct/lens foo (a b c) #:transparent)
    (lens-view foo-a-lens (foo 1 2 3))
    (lens-set foo-a-lens (foo 1 2 3) 100)
]}

@defform[(struct-lenses-out struct-id)]{
A @racket[provide] sub-form that provides the lenses defined by
@racket[define-struct-lenses] or @racket[struct/lens].
}

@defform[(struct+lenses-out struct-id)]{
A @racket[provide] sub-form short for using both @racket[struct-out] and
@racket[struct-lenses-out].
}
