#lang scribble/manual

@(require lens/private/doc-util/main)

@title[#:tag "built-in-lenses" #:style 'toc]{Lenses on Built-In Datatypes}

This library provides @lens-tech{lenses} for most built-in Racket datatypes. In general, the name of
each lens corresponds to the name of its accessor function with @racket[-lens] appended to the end.
For example, the lens for accessing the first element of a pair is @racket[car-lens], and the lens for
accessing an element of a hash is called @racket[hash-ref-lens].

@local-table-of-contents[]

@include-section["built-in/ordered.scrbl"]
@include-section["built-in/key-value.scrbl"]
