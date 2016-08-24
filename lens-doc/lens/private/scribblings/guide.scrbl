#lang scribble/manual

@title[#:tag "lens-guide" #:style 'toc]{The Lens Guide}

This guide is intended for programmers who are familiar with Racket but new to working with lenses or
a certain part of this lens library. It contains a non-authorative introduction to lenses, including
examples of usage and recipes for solving certain kinds of problems. It does not describe all features
this library provides; for a complete API reference, see @secref{lens-reference}.

@local-table-of-contents[]

@include-section["guide/introduction.scrbl"]
@include-section["guide/built-in.scrbl"]
@include-section["guide/user-defined.scrbl"]
