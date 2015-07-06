#lang scribble/manual

@(require scribble/eval
          (for-label lenses
                     racket/contract
                     racket/base))

@(define lenses-eval (make-base-eval))
@(lenses-eval '(require "main.rkt"))
@(lenses-eval '(require racket/list))
@(define-syntax-rule (lenses-examples datum ...)
   (examples #:eval lenses-eval datum ...))

@(define lenses-applicable-eval (make-base-eval))
@(lenses-applicable-eval '(require "applicable.rkt"))
@(lenses-applicable-eval '(require racket/list))
@(define-syntax-rule (lenses-applicable-examples datum ...)
   (examples #:eval lenses-applicable-eval datum ...))

@title{Lenses}

@defmodule[lenses]

This library includes functions and forms for working with @italic{lenses}.
A lens is a value that operates on some small piece of a larger
structure. Think of them as a more general representation of getters and
setters in object-oriented languages.

@author[@author+email["Jack Firth" "jackhfirth@gmail.com"]]

source code: @url["https://github.com/jackfirth/lenses"]

@section{Core Lens Forms}

@defproc[(lens? [v any/c]) boolean?]{
  Predicate for lenses.
}

@defproc[(make-lens [getter (-> target/c view/c)]
                    [setter (-> target/c view/c target/c)])
         lens?]{
  Given a getter and a setter, constructs a lens defined on values
  satisfying @racket[target/c] and viewing values satisfying
  @racket[view/c]. The getter must accept a target and return the
  lens's view. The setter must accept a target and a new view, and
  return a new target with its view replaced with the new view.
  @lenses-examples[
    (define (set-first lst v)
      (list* v (rest lst)))
    (set-first '(1 2 3) 'a)
    (define first-lens (make-lens first set-first))
    (lens-view first-lens '(1 2 3))
    (lens-set first-lens '(1 2 3) 'a)
]}

@defform[(let-lens (view-id context-id) lens-expr target-expr body ...)]{
  Gets a lens and a target, constructs the @italic{view} and the
  @italic{context} of the target through the lens and binds them
  to @racket[view-id] and @racket[context-id] respectively. The
  @italic{context} is a function that accepts a new view and sets
  the target's view to the new view. The context is conceptually
  a function representing the "hole" formed by abstracting the view
  of the target.
  @lenses-examples[
    (let-lens (view context) first-lens '(1 2 3)
      (printf "View is ~a\n" view)
      (context 'a))
]}

@defproc[(lens-view [lens lens?] [target target/c]) view/c]{
  Extracts the view of @racket[target] with @racket[lens].
  Essentially a getter function.
  @lenses-examples[
    (lens-view first-lens '(1 2 3))
]}

@defproc[(lens-set [lens lens?] [target target/c] [new-view view/c]) target/c]{
  Sets the view of @racket[target] to @racket[new-view] using
  @racket[lens]. Essentially a setter function.
  @lenses-examples[
    (lens-set first-lens '(1 2 3) 'a)
]}

@defproc[(lens-transform [lens lens?]
                         [transformer (-> view/c view/c)]
                         [target target/c])
         target/c]{
  Transforms the view of @racket[target] through the given @racket[lens]
  with the @racket[transformer] function. Equivalent to getting the
  view of @racket[target] through @racket[lens], passing that value
  to @racket[transformer], then setting the view of @racket[target]
  to the return value of calling @racket[transformer] with the old
  view.
  @lenses-examples[
    (lens-transform first-lens number->string '(1 2 3))
]}

@defproc[(lens-compose [lens lens?] ...) lens?]{
  Composes the given lenses together into one @italic{compound lens}.
  The compound lens operates similarly to composed functions do in
  that the last @racket[lens] is the first @racket[lens] the compound
  lens's target is viewed through. Each successive lens "zooms in"
  to a more detailed view.
  @lenses-examples[
    (define first-of-second-lens (lens-compose first-lens second-lens))
    (lens-view first-of-second-lens '((1 a) (2 b) (3 c)))
    (lens-set first-of-second-lens '((1 a) (2 b) (3 c)) 200)
]}

@defthing[identity-lens lens?]{
  The identity lens. Performs no destructuring at all - it's view is
  the target itself. For all lenses, both
  @racket[(lens-compose lens identity-lens)] and
  @racket[(lens-compose identity-lens lens)] are equivalent to
  @racket[lens].
  @lenses-examples[
    (lens-view identity-lens 4)
    (lens-set identity-lens 4 'a)
]}

@section{List lenses}

@defproc[(list-lens [n exact-nonnegative-integer?])
         lens?]{
  Returns a lens for viewing the @racket[n]th item of a list,
  with indexing starting from zero.
  @lenses-examples[
    (lens-view (list-lens 3) '(a b c d e f g h))
    (lens-set (list-lens 1) '(a b c d e f g h) 'FOO)
]}

@deftogether[(
  @defthing[first-lens lens?]
  @defthing[second-lens lens?]
  @defthing[third-lens lens?]
  @defthing[fourth-lens lens?]
  @defthing[fifth-lens lens?]
  @defthing[sixth-lens lens?]
  @defthing[seventh-lens lens?]
  @defthing[eighth-lens lens?]
  @defthing[ninth-lens lens?]
  @defthing[tenth-lens lens?])]{
    Lenses for examiniming specific items of lists. Shorthands
    for the common use cases of @racket[list-lens].
    @lenses-examples[
      (lens-view third-lens '(a b c d))
      (lens-view (lens-compose second-lens fourth-lens)
                 '((a 1) (b 2) (c 3) (d 4)))
]}

@defproc[(assoc-lens [key any/c] [#:is-equal? key-equal? (-> any/c any/c any/c) equal?])
         lens?]{
  Constructs a lens for examiniming association lists.
  Specifically, for a given association list the returned
  lens examines the second value of the first pair that
  has a key that is @racket[key-equal?] to @racket[key].
  @lenses-examples[
    (define assoc-a-lens (assoc-lens 'a))
    (define some-assoc-list '((a . 1) (b . 2) (c . 3)))
    (lens-view assoc-a-lens some-assoc-list)
    (lens-set assoc-a-lens some-assoc-list 100)
  ]
  
  The @racket[key-equal?] procedure is useful for
  datatypes that have their own definition of
  equality, such as strings.
  @lenses-examples[
    (define assoc-foo-lens (assoc-lens "foo" #:is-equal? string=?))
    (lens-view assoc-foo-lens '(("bar" 1) ("foo" 2) ("baz" 3)))
]}

@defproc[(assv-lens [key any/c]) lens?]{
  Equivalent to @racket[(assoc-lens key #:is-equal? eqv?)].
}

@defproc[(assq-lens [key any/c]) lens?]{
  Equivalent to @racket[(assoc-lens key #:is-equal? eq?)].
}

@section{Syntax lenses}

@defform[(syntax-lens target-id structure)]{
  Constructs a lens that parses a syntax object and returns
  a piece of that syntax object as determined by where
  @racket[target-id] appears in @racket[structure].
  @lenses-examples[
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
  @lenses-examples[
    (define foo-kw-seq-lens (syntax-keyword-seq-lens '#:foo))
    (lens-view foo-kw-seq-lens #'(a #:foo c d #:bar f))
    (lens-set foo-kw-seq-lens #'(a #:foo c d #:bar f) #'(1 2 3 4 5 6))
  ]
  
  If the target syntax object has no occurence of @racket[kw],
  or if the occurence of @racket[kw] is at the end of the syntax
  object or immediately followed by another keyword, then viewing
  produces the empty list syntax object @racket[#'()]. In the case
  where @racket[kw] is not present, setting is a no-op.
  @lenses-examples[
    (lens-view foo-kw-seq-lens #'(a b f g))
    (lens-view foo-kw-seq-lens #'(a #:foo #:bar f))
    (lens-set foo-kw-seq-lens #'(a #:foo #:bar f) #'(1 2 3 4 5 6))
    (lens-set foo-kw-seq-lens #'(a b f g) #'(these are ignored))
]}

@section{lenses/applicable}

@defmodule[lenses/applicable]

This module provides the same functions as @racketmodname[lenses],
but enables the use of @italic{applicable lenses}. Applicable lenses
may be used directly as getter functions, removing the need to use
@racket[lens-view].

@lenses-applicable-examples[
  (require lenses/applicable)
  (first-lens '(a b c))
  (map first-lens '((1 2 3) (a b c) (100 200 300)))
]

Attempting to use non-applicable lenses as functions is an error.

@lenses-examples[
  (require lenses)
  (first-lens '(a b c))
]
