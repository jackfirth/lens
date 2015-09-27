#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Struct-lens provide forms}

@defmodule[unstable/lens/struct-provide]

@defform[(struct-lenses-out struct-id)]{
A @racket[provide] sub-form that provides the lenses defined by
@racket[define-struct-lenses] or @racket[struct/lens].
}

@defform[(struct+lenses-out struct-id)]{
A @racket[provide] sub-form short for using both @racket[struct-out] and
@racket[struct-lenses-out].
}
