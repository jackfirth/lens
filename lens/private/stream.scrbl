#lang scribble/manual

@(require "doc-util/main.rkt")


@title{Stream Lenses}

@defthing[stream-first-lens lens?]{
A lens for viewing the first element of a stream.
@lenses-examples[
  (lens-view stream-first-lens (stream 1 2 3))
  (stream->list (lens-set stream-first-lens (stream 1 2 3) 'a))
]}

@defthing[stream-rest-lens lens?]{
A lens for viewing the rest of a stream after the first element.
@lenses-examples[
  (stream->list (lens-view stream-rest-lens (stream 1 2 3)))
  (stream->list (lens-set stream-rest-lens (stream 1 2 3) (stream 200 300 400 500)))
]}

@defproc[(stream-ref-lens [i exact-nonnegative-integer?]) lens?]{
A lens for viewing the @racket[i]th element of a stream.
@lenses-examples[
  (lens-view (stream-ref-lens 2) (stream 1 2 3 4 5 6))
  (stream->list (lens-set (stream-ref-lens 2) (stream 1 2 3 4 5 6) 'a))
]}
