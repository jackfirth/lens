#lang scribble/manual

@(require "doc-util/main.rkt")


@title{Joining Lenses}

@defproc[(lens-join/list [lens lens?] ...) lens?]{
  Constructs a lens that combines the view of each
  @racket[lens] into a list of views. This lens can
  be used to view and set a list of values in a single
  target. If any of the lenses share views, then when
  setting the later lenses override the earlier ones.
  @lenses-examples[
    (define first-third-fifth-lens
      (lens-join/list first-lens
                      third-lens
                      fifth-lens))
    (lens-view first-third-fifth-lens '(a b c d e f))
    (lens-set first-third-fifth-lens '(a b c d e f) '(1 2 3))
]}

@defproc[(lens-join/hash [key any/c] [lens lens?] ... ...) lens?]{
  Constructs a lens that combines the view of each
  @racket[lens] into a hash of views with @racket[key]s
  as the hash keys. In the same manner as @racket[lens-join/list],
  if lenses share views later lenses take precedence when
  setting.
  @lenses-examples[
    (define a-b-lens (lens-join/hash 'a first-lens
                                     'b third-lens))
    (lens-view a-b-lens '(1 2 3))
    (lens-set a-b-lens '(1 2 3) (hash 'a 100 'b 200))
]}

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

@defproc[(lens-join/string [lens lens?] ...) lens?]{
  Like @racket[lens-join/list], except the view is a string, not a list.
  Each @racket[lens] argument must return a @racket[char?] as a view.
  @lenses-examples[
    (define string-first-third-fifth-lens
      (lens-join/string first-lens
                        third-lens
                        fifth-lens))
    (lens-view string-first-third-fifth-lens '(#\a #\b #\c #\d #\e #\f))
    (lens-set string-first-third-fifth-lens '(#\a #\b #\c #\d #\e #\f) "ACE")
]}
