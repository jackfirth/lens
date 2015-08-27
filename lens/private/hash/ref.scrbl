#lang scribble/manual

@(require "../doc-util/main.rkt")


@defproc[(hash-ref-lens [key any/c]) lens?]{
  Constructs a lens that targets hashes and views the value
  of @racket[key].
  @lens-examples[
    (define foo-lens (hash-ref-lens 'foo))
    (lens-view foo-lens (hash 'foo 10 'bar 20))
    (lens-set foo-lens (hash 'foo 10 'bar 20) 1000)
]}
