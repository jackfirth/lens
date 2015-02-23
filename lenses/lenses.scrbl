#lang scribble/manual

@(require scribble/eval
          (for-label lenses
                     racket/base))

@(define lenses-eval (make-base-eval))
@(lenses-eval '(require "main.rkt"))
@(define-syntax-rule (lenses-examples datum ...)
   (examples #:eval lenses-eval datum ...))

@title{Lenses}

@defmodule[lenses]

This library includes functions and forms for working with @italic{lenses}.
A lens is a pure function that operates on some small piece of a larger
structure. Think of them as a more general representation of getters and
setters in object-oriented languages.

@author[@author+email["Jack Firth" "jackhfirth@gmail.com"]]

source code: @url["https://github.com/jackfirth/lenses"]
