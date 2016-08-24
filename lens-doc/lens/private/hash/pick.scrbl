#lang scribble/manual

@(require lens/private/doc-util/main)


@defproc[(hash-pick-lens [key any/c] ...) lens?]{
  Creates a lens that views a subset of the target hash-table with the given
  @racket[key]s. The view, is another hash-table with only the given keys and
  their corrosponding values in the target hash-table.
  @lens-examples[
    (lens-view (hash-pick-lens 'a 'c) (hash 'a 1 'b 2 'c 3))
    (lens-set (hash-pick-lens 'a 'c) (hash 'a 1 'b 2 'c 3) (hash 'a 4 'c 5))
]}
