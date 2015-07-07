#lang scribble/manual

@title{Lenses}

@defmodule[lenses]

This library includes functions and forms for working with @italic{lenses}.
A lens is a value that operates on some small piece of a larger
structure. Think of them as a more general representation of getters and
setters in object-oriented languages.

@author[@author+email["Jack Firth" "jackhfirth@gmail.com"]]

source code: @url["https://github.com/jackfirth/lenses"]

@include-section["core/main.scrbl"]
@include-section["list/main.scrbl"]
@include-section["struct-lens.scrbl"]
@include-section["syntax.scrbl"]
@include-section["applicable.scrbl"]
