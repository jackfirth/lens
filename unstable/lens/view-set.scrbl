#lang scribble/manual

@(require lens/private/doc-util/main)


@title{More Viewing and Setting}

@defproc[(lens-set-all [target any/c] [new-view any/c] [lens lens?] ...) any/c]{
  Sets the view of @racket[target] through each @racket[lens] to @racket[new--view]
  @lens-unstable-examples[
    (lens-set-all '(1 2 3 4 5) 'a
                  first-lens
                  third-lens
                  fourth-lens)
]}
