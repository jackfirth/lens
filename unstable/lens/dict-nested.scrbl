#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Lenses for nested dictionaries}

@defmodule[unstable/lens/dict-nested]

@defproc[(dict-ref-nested-lens [k any/c] ...) (lens/c functional-dict? any/c)]{
Similar to @racket[hash-ref-nested-lens], but for dicts.
@lens-unstable-examples[
  (define a-x (dict-ref-nested-lens 'a 'x))
  (lens-view a-x '([a . ([x . 1] [y . 2])] '[b . ([z . 3])]))
  (lens-set a-x '([a . ([x . 1] [y . 2])] '[b . ([z . 3])]) 100)
]}
