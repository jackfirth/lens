#lang scribble/manual

@title{Lenses}

@defmodule[lens]

This library includes functions and forms for working with
@deftech[#:key "lens"]{lenses}. A lens is a value that operates on some
small piece of a larger structure. Think of them as a more general
representation of getters and setters in object-oriented languages.

@author[@author+email["Jack Firth" "jackhfirth@gmail.com"]
        @author+email["Alex Knauth" "alexander@knauth.org"]]

source code: @url["https://github.com/jackfirth/lens"]

@include-section["base/main.scrbl"]
@include-section["list/main.scrbl"]
@include-section["struct.scrbl"]
@include-section["dict.scrbl"]
@include-section["applicable.scrbl"]
