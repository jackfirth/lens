#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Unstable Lenses}

@defmodule[unstable/lens]

This library provides additional features for the
@racketmodname[lens] library that are non-final and
may change in future releases. Do not depend on
this library being backwards-compatible.

@(include-sections
  (lib "lens/private/list/append.scrbl")
  "arrow.scrbl"
  "define-nested.scrbl"
  "dict-nested.scrbl"
  "hash-filterer.scrbl"
  "if.scrbl"
  "isomorphism.scrbl"
  "join-assoc.scrbl"
  "lazy.scrbl"
  "map.scrbl"
  "match.scrbl"
  "set-filterer.scrbl"
  "set-member.scrbl"
  "string-split.scrbl"
  "struct-join.scrbl"
  "struct-list.scrbl"
  "struct-nested.scrbl"
  "struct-provide.scrbl"
  "sublist.scrbl"
  "substring.scrbl"
  "syntax.scrbl"
  "set-all.scrbl"
  "zoom.scrbl"
  )

