#lang scribble/manual

@(require "../doc-util/main.rkt")


@defproc[(lens-join/vector [lens lens?] ...) lens?]{
  Like @racket[lens-join/list], except the view is a vector, not a list.
  @lenses-examples[
    (define vector-first-third-fifth-lens
      (lens-join/vector first-lens
                        third-lens
                        fifth-lens))
    (lens-view vector-first-third-fifth-lens '(a b c d e f))
    (lens-set vector-first-third-fifth-lens '(a b c d e f) #(1 2 3))
]}
