#lang scribble/manual

@(require lens/private/doc-util/main)


@title[#:tag "dict-reference"]{Dict lenses}

@see-guide-note["dict-guide"]{dictionary lenses}

@defproc[(dict-ref-lens [key any/c]) lens?]{
  Returns a lens for viewing the value mapped to @racket[key] in a dict.
  @lens-examples[
    (define dict '((a . 1) (b . 2) (c . 3)))
    (lens-view (dict-ref-lens 'a) dict)
    (lens-set (dict-ref-lens 'a) dict 100)
]}
