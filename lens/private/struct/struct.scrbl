#lang scribble/manual

@(require "../doc-util/main.rkt")


@defform[(define-struct-lenses struct-id)]{
  Given a @racket[struct-id], defines a lens for each of its fields.
  Additionally, defines a syntax transformer of the form @racket[struct-lens:struct-id]
  which contains a @racket[struct-lens-info] instance containing the
  bound field lens identifiers.
  @lens-examples[
    (struct foo (a b c) #:transparent)
    (define-struct-lenses foo)
    (lens-view foo-a-lens (foo 1 2 3))
    (lens-set foo-a-lens (foo 1 2 3) 100)
]}

@defstruct*[struct-lens-info ([field-lens-ids (listof identifier?)])]{
  A struct to hold information about the lenses created by @racket[define-struct-lenses].
  Used at compile time to reflect on the structs lenses. This binding is provided
  @racket[for-syntax].
}

@defform[(struct/lens struct-id (field-spec ...) struct-option ...)]{
  Equivalent to @racket[struct] and @racket[define-struct-lenses] combined.
  @lens-examples[
    (struct/lens foo (a b c) #:transparent)
    (lens-view foo-a-lens (foo 1 2 3))
    (lens-set foo-a-lens (foo 1 2 3) 100)
]}
