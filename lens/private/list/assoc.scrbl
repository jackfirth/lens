#lang scribble/manual

@(require "../doc-util/main.rkt")


@title{Association List Lenses}

@defproc[(assoc-lens [key any/c] [#:is-equal? key-equal? (-> any/c any/c any/c) equal?])
         lens?]{
  Constructs a lens for examiniming association lists.
  Specifically, for a given association list the returned
  lens examines the second value of the first pair that
  has a key that is @racket[key-equal?] to @racket[key].
  @lens-examples[
    (define assoc-a-lens (assoc-lens 'a))
    (define some-assoc-list '((a . 1) (b . 2) (c . 3)))
    (lens-view assoc-a-lens some-assoc-list)
    (lens-set assoc-a-lens some-assoc-list 100)
  ]
  
  The @racket[key-equal?] procedure is useful for
  datatypes that have their own definition of
  equality, such as strings.
  @lens-examples[
    (define assoc-foo-lens (assoc-lens "foo" #:is-equal? string=?))
    (lens-view assoc-foo-lens '(("bar" . 1) ("foo" . 2) ("baz" . 3)))
]}

@defproc[(assv-lens [key any/c]) lens?]{
  Equivalent to @racket[(assoc-lens key #:is-equal? eqv?)].
}

@defproc[(assq-lens [key any/c]) lens?]{
  Equivalent to @racket[(assoc-lens key #:is-equal? eq?)].
}
