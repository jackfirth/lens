#lang scribble/manual

@(require scribble/eval
          (for-label lenses
                     racket/contract
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

@section{Core Lens Forms}

@defproc[(lens/c [target/c contract?] [view/c contract?]) contract?]{
  Contract constructor for lenses. A lens is a function that takes one
  value, its @italic{target}, and returns two values, a @italic{view}
  and a @italic{context}. The context is a function that takes a new view
  value and "replaces" the old view value with the new value, giving a
  modified target. Less technically, a lens is a way to analyze some
  specific piece of a @racket[target/c] that is a @racket[view/c],
  along with a way to replace that piece with a new view value. Lenses
  deconstruct and reconstruct data by examinimg small portions of their
  structure. In terms of contracts, a @racket[(lens/c target/c view/c)]
  is equivalent to the following function contract:
  @racketblock[
    (-> target/c
        (values view/c
                (-> view/c target/c)))
  ]
  
  An example is the @racket[first-lens], which is a lens for examiniming
  specifically the first item in a list:
  @lenses-examples[
    (first-lens '(1 2 3))
    (let-values ([(_ context) (first-lens '(1 2 3))])
      (context 'a))
]}

@defform[(let-lens (view-id context-id) lens-call-expr body ...)]{
  Restricted form of @racket[let-values] specifically for working with
  the return values of a lens function. This is purely for semantic
  clarity and to eliminate a few extra parens.
  @lenses-examples[
    (let-lens (view context) (first-lens '(1 2 3))
      (printf "View is ~a\n" view)
      (context 'a))
]}

@defproc[(lens-view [lens (lens/c target/c view/c)] [target target/c]) view/c]{
  Extracts only the view of @racket[target] with @racket[lens], disregarding
  the context. Essentially a getter function.
  @lenses-examples[
    (lens-view first-lens '(1 2 3))
]}

@defproc[(lens-set [lens (lens/c target/c view/c)] [target target/c] [new-view view/c]) target/c]{
  Sets the view of @racket[target] to @racket[new-view] using @racket[lens].
  Shorthand for getting the context of @racket[target] with @racket[lens],
  then calling that context function with @racket[new-view]. Essentially
  a setter function.
  @lenses-examples[
    (lens-set first-lens '(1 2 3) 'a)
]}

@defproc[(lens-transform [lens (lens/c target/c view/c)]
                         [target target/c]
                         [transformer (-> view/c view/c)])
         target/c]{
  Transforms the view of @racket[target] through the given @racket[lens]
  with the @racket[transformer] function. Equivalent to getting the
  view of @racket[target] through @racket[lens], passing that value
  to @racket[transformer], then setting the view of @racket[target]
  to the return value of calling @racket[transformer] with the old
  view.
  @lenses-examples[
    (lens-transform first-lens '(1 2 3) number->string)
]}

@defproc[(lens-compose [lens proc] ...+) proc?]{
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

@section{List lenses}

@defproc[(list-lens [n exact-nonnegative-integer?])
         (lens/c list? any?)]{
  Returns a lens for examining the @racket[n]th item of a list,
  with indexing starting from zero.
  @lenses-examples[
    (lens-view (list-lens 3) '(a b c d e f g h))
    (lens-set (list-lens 1) '(a b c d e f g h) 'FOO)
]}

@deftogether[(
  @defthing[first-lens (lens/c list? any/c)]
  @defthing[second-lens (lens/c list? any/c)]
  @defthing[third-lens (lens/c list? any/c)]
  @defthing[fourth-lens (lens/c list? any/c)]
  @defthing[fifth-lens (lens/c list? any/c)])]{
    Lenses for examiniming specific items of lists. Shorthands
    for the common use cases of @racket[list-lens].
    @lenses-examples[
      (lens-view third-lens '(a b c d))
      (lens-view (lens-compose second-lens fourth-lens)
                 '((a 1) (b 2) (c 3) (d 4)))
]}

@defproc[(assoc-lens [key any/c] [#:is-equal? key-equal? (-> any/c any/c any/c) equal?])
         (lens/c (listof pair?) any/c)]{
  Constructs a lens for examiniming association lists.
  Specifically, for a given association list the returned
  lens examines the second value of the first pair that
  has a key that is @racket[key-equal?] to @racket[key].
  @lenses-examples[
    (define assoc-a-lens (assoc-lens 'a))
    (define some-assoc-list '((a 1) (b 2) (c 3)))
    (lens-view assoc-a-lens some-assoc-list)
    (lens-set assoc-a-lens some-assoc-list 100)
  ]
  
  If no key in the association list exists that is
  @racket[key-equal?] to @racket[key], then attempting
  to view an association list with the lens returns
  @racket[#f] and setting a view appends a new pair
  to the end of the association list
  @lenses-examples[
    (define assoc-d-lens (assoc-lens 'd))
    (lens-view assoc-d-lens some-assoc-list)
    (lens-set assoc-d-lens some-assoc-list 100)
  ]
  
  The @racket[key-equal?] procedure is useful for
  datatypes that have their own definition of
  equality, such as strings.
  @lenses-examples[
    (define assoc-foo-lens (assoc-lens "foo" #:is-equal? string=?))
    (lens-view assoc-foo-lens '(("bar" 1) ("foo" 2) ("baz" 3)))
]}