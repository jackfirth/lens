#lang scribble/manual

@(require "../doc-util/main.rkt")


@defproc[(lens-join/string [lens lens?] ...) lens?]{
  Like @racket[lens-join/list], except the view is a string, not a list.
  Each @racket[lens] argument must return a @racket[char?] as a view.
  @lens-examples[
    (define string-first-third-fifth-lens
      (lens-join/string first-lens
                        third-lens
                        fifth-lens))
    (lens-view string-first-third-fifth-lens '(#\a #\b #\c #\d #\e #\f))
    (lens-set string-first-third-fifth-lens '(#\a #\b #\c #\d #\e #\f) "ACE")
]}
