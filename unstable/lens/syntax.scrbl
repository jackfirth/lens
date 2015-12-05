#lang scribble/manual

@(require lens/private/doc-util/main)


@title{Syntax Lenses}

@defform[(syntax-lens target-id structure)]{
  Constructs a lens that parses a syntax object and returns
  a piece of that syntax object as determined by where
  @racket[target-id] appears in @racket[structure].
  @lens-unstable-examples[
    (define first-of-second-stx-lens
      (syntax-lens A
        (_ (A _ ...) _ ...)))
    (lens-view first-of-second-stx-lens
               #'(foo (1 2 3) bar baz (blah blah)))
    (lens-set first-of-second-stx-lens
              #'(define (f a) a)
              #'g)
]}

@defproc[(syntax-keyword-seq-lens [kw keyword?])
         lens?]{
  Constructs a lens that examines a non-flat syntax object
  and views a syntax object containing all the terms in the
  target syntax that appear after @racket[kw] but before any
  other keyword.
  @lens-unstable-examples[
    (define foo-kw-seq-lens (syntax-keyword-seq-lens '#:foo))
    (lens-view foo-kw-seq-lens #'(a #:foo c d #:bar f))
    (lens-set foo-kw-seq-lens #'(a #:foo c d #:bar f) #'(1 2 3 4 5 6))
  ]

  If the target syntax object has no occurence of @racket[kw],
  or if the occurence of @racket[kw] is at the end of the syntax
  object or immediately followed by another keyword, then viewing
  produces the empty list syntax object @racket[#'()]. In the case
  where @racket[kw] is not present, setting is a no-op.
  @lens-unstable-examples[
    (define foo-kw-seq-lens (syntax-keyword-seq-lens '#:foo))
    (lens-view foo-kw-seq-lens #'(a b f g))
    (lens-view foo-kw-seq-lens #'(a #:foo #:bar f))
    (lens-set foo-kw-seq-lens #'(a #:foo #:bar f) #'(1 2 3 4 5 6))
    (lens-set foo-kw-seq-lens #'(a b f g) #'(these are ignored))
]}
