#lang scribble/manual

@title[#:style '(toc)]{Lenses}

@defmodule[lens]

This library includes functions and forms for working with
@deftech[#:key "lens"]{lenses}. A lens is a value that operates on some
small piece of a larger structure. Think of them as a more general
representation of getters and setters in object-oriented languages.

@author[@author+email["Jack Firth" "jackhfirth@gmail.com"]
        @author+email["Alex Knauth" "alexander@knauth.org"]]

source code: @url["https://github.com/jackfirth/lens"]

@local-table-of-contents[]

@include-section["private/base/main.scrbl"]
@include-section["private/compound/main.scrbl"]
@include-section["private/list/main.scrbl"]
@include-section["private/hash/main.scrbl"]
@include-section["private/struct/main.scrbl"]
@include-section["private/vector/main.scrbl"]
@include-section["private/string.scrbl"]
@include-section["private/stream.scrbl"]
@include-section["private/dict.scrbl"]
@include-section["applicable.scrbl"]
