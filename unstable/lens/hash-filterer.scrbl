#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Filtering hash-tables}

@defmodule[unstable/lens/hash-filterer]

@defproc[(hash-filterer-lens/key [keep? (-> any/c boolean?)])
         (lens/c immutable-hash? immutable?)]{
Creates a lens that filters a hash-table to keys that pass the predicate
@racket[keep?].
@lens-unstable-examples[
  (lens-view (hash-filterer-lens/key symbol?) (hash 'a 1 "b" 2 'c 3))
  (lens-set (hash-filterer-lens/key symbol?) (hash 'a 1 "b" 2 'c 3) (hash 'd 4 'e 5))
]}

@defproc[(hash-filterer-lens/value [keep? (-> any/c boolean?)])
         (lens/c immutable-hash? immutable?)]{
Like @racket[hash-filterer-lens/value], but filters based on values that pass
@racket[keep?], not keys.
@lens-unstable-examples[
  (lens-view (hash-filterer-lens/value number?) (hash 'a 1 'b "two" 'c 3))
  (lens-set (hash-filterer-lens/value number?) (hash 'a 1 'b "two" 'c 3) (hash 'd 4))
]}

@defproc[(hash-filterer-lens [keep? (-> any/c any/c boolean?)])
         (lens/c immutable-hash? immutable?)]{
Creates a lens that filters a hash-table by the predicate @racket[keep?], which
takes the key and the value as its two arguments.
@lens-unstable-examples[
  (lens-view (hash-filterer-lens =) (hash 1 1.0 2 45 3 3))
  (lens-set (hash-filterer-lens =) (hash 1 1.0 2 45 3 3) (hash 4 4.0 5.0 5))
]}

