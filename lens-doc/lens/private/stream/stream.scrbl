#lang scribble/manual

@(require lens/private/doc-util/main)


@title[#:tag "streams-reference"]{Stream Lenses}

@defmodule[lens/data/stream]

@see-guide-note["streams-guide"]{stream lenses}

@defthing[stream-first-lens lens?]{
A lens for viewing the first element of a stream.
@lens-examples[
  (lens-view stream-first-lens (stream 1 2 3))
  (stream->list (lens-set stream-first-lens (stream 1 2 3) 'a))
]}

@defthing[stream-rest-lens lens?]{
A lens for viewing the rest of a stream after the first element.
@lens-examples[
  (stream->list (lens-view stream-rest-lens (stream 1 2 3)))
  (stream->list (lens-set stream-rest-lens (stream 1 2 3) (stream 200 300 400 500)))
]}

@defproc[(stream-ref-lens [i exact-nonnegative-integer?]) lens?]{
A lens for viewing the @racket[i]th element of a stream.
@lens-examples[
  (lens-view (stream-ref-lens 2) (stream 1 2 3 4 5 6))
  (stream->list (lens-set (stream-ref-lens 2) (stream 1 2 3 4 5 6) 'a))
]}
