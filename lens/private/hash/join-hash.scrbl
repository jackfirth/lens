#lang scribble/manual

@(require "../doc-util/main.rkt")


@defproc[(lens-join/hash [key any/c] [lens lens?] ... ...) lens?]{
  Constructs a lens that combines the view of each
  @racket[lens] into a hash of views with @racket[key]s
  as the hash keys. In the same manner as @racket[lens-join/list],
  if lenses share views later lenses take precedence when
  setting.
  @lens-examples[
    (define first-third-hash-lens
      (lens-join/hash 'first first-lens
                      'third third-lens))
    (lens-view first-third-hash-lens '(1 2 3))
    (lens-set first-third-hash-lens '(1 2 3) (hash 'first 100 'third 200))
]}
