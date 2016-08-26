#lang scribble/manual

@(require lens/private/doc-util/main)

@title[#:style '(toc)]{Lenses}

@defmodule[lens]

This library includes functions and forms for working with
@lens-tech{lenses}. A lens is a value that operates on some small piece
of a larger structure. Think of them as a more general representation
of getters and setters in object-oriented languages.

@author[@author+email["Jack Firth" "jackhfirth@gmail.com"]
        @author+email["Alex Knauth" "alexander@knauth.org"]]

source code: @url["https://github.com/jackfirth/lens"]

@stability-notice[unstable/lens]

@local-table-of-contents[]

@include-section["private/scribblings/guide.scrbl"]
@include-section["private/scribblings/reference.scrbl"]
@include-section[(lib "unstable/lens/main.scrbl")]
