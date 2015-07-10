#lang scribble/manual

@(require "../doc-util/main.rkt")


@title{Viewing and Setting}

@defproc[(lens-view [lens lens?] [target target/c]) view/c]{
  Extracts the view of @racket[target] with @racket[lens].
  Essentially a getter function.
  @lenses-examples[
    (lens-view first-lens '(1 2 3))
]}

@defproc[(lens-set [lens lens?] [target target/c] [new-view view/c]) target/c]{
  Sets the view of @racket[target] to @racket[new-view] using
  @racket[lens]. Essentially a setter function.
  @lenses-examples[
    (lens-set first-lens '(1 2 3) 'a)
]}

@defproc[(lens-view/list [target target/c] [lens lens?] ...) view/c]{
  Like @racket[lens-view], except that it takes multiple lenses and
  returns a list of views.
  @lenses-examples[
    (lens-view/list '(a b c d e f g)
                    first-lens fourth-lens fifth-lens)
]}

@defproc[(lens-set/list [target target/c] [lens lens?] [new-view view/c] ... ...) target/c]{
  Like @racket[lens-set], except that it can take multiple
  lenses-value pairs. If the view of two of the lenses overlap, the
  later views overwrite the earlier ones.
  @lenses-examples[
    (lens-set/list '(1 2 3 4 5)
                   first-lens 10
                   third-lens 300)
    (lens-set/list '(1 2 3)
                   first-lens 'a
                   first-lens 'b)
]}
