#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Joining lenses with an association list}

@defproc[(lens-join/assoc [key key/c] [lens (lens/c target/c value/c)] ... ...)
         (lens/c target/c (listof (cons/c key/c value/c)))]{
Like @racket[lens-join/hash], except joins the keys and values into an
association list instead of a hash-table.
@lens-unstable-examples[
  (define a-b-lens (lens-join/assoc 'a first-lens
                                    'b third-lens))
  (lens-view a-b-lens '(1 2 3))
  (lens-set a-b-lens '(1 2 3) '((a . 100) (b . 200)))
]}
