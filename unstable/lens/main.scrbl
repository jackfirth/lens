#lang scribble/manual

@title{Unstable Lenses}

@defmodule[unstable/lens]

This library provides additional features for the
@racketmodname[lens] library that are non-final and
may change in future releases. Do not depend on
this library being backwards-compatible.

@include-section["compound.scrbl"]
@include-section["list.scrbl"]
@include-section["hash.scrbl"]
@include-section["syntax.scrbl"]
