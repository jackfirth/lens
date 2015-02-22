#lang scribble/manual

@(require scribble/eval
          (for-label lenses
                     racket/base))

@title{Lenses}

@(define lenses-eval (make-base-eval))
@(lenses-eval '(require "main.rkt"))
@(define-syntax-rule (lenses-examples datum ...)
   (examples #:eval lenses-eval datum ...))

@defmodule[lenses]

@author[@author+email["Jack Firth" "jackhfirth@gmail.com"]]

source code: @url["https://github.com/jackfirth/lenses"]