#lang scribble/manual

@(require lens/doc-util/main)

@title{Lenses based on match patterns}

@defform[(match-lens id pattern replacement)]{
Creates a lens for viewing the @racket[id] within the @racket[pattern].

The @racket[replacement] expression should be an expression such that
@racket[(match target [pattern replacement])] produces a value equivalent to
@racket[target], and should use @racket[id] as the view.
@lenses-unstable-examples[
  (define car-lens (match-lens a (cons a b) (cons a b)))
  (define cdr-lens (match-lens b (cons a b) (cons a b)))
  (define third-lens (match-lens c (list a b c d ...) (list* a b c d)))
  (define vector-second-lens (match-lens b (vector a b c ...) (apply vector a b c)))
  (define v2-of-l3-lens (match-lens d
                          (list a b (vector c d e ...) f ...)
                          (list* a b (apply vector c d e) f)))
]}
