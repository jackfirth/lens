#lang scribble/manual

@title[#:tag "user-defined-lenses" #:style 'toc]{Lenses on User-Defined Datatypes}

In addition to the built-in lenses, @racketmodname[lens] provides utilities for construcing new lenses
for user-defined datatypes. This section covers the utilities for creating lenses for Racket structs,
as well as the general lens constructors for making arbitrary lenses.

@local-table-of-contents[]

@include-section["user-defined/struct.scrbl"]
@include-section["user-defined/custom.scrbl"]
