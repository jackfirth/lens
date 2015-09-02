#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Unstable Lenses}

@defmodule[unstable/lens]

This library provides additional features for the
@racketmodname[lens] library that are non-final and
may change in future releases. Do not depend on
this library being backwards-compatible.

@(include-sections
  "arrow.scrbl"
  "dict-nested.scrbl"
  "if.scrbl"
  "isomorphism.scrbl"
  "join-assoc.scrbl"
  "lazy.scrbl"
  "mapper.scrbl"
  "match.scrbl"
  "set-filterer.scrbl"
  "set-member.scrbl"
  "string-split.scrbl"
  "struct-join.scrbl"
  "struct-nested.scrbl"
  "sublist.scrbl"
  "syntax.scrbl"
  "view-set.scrbl"
  )

