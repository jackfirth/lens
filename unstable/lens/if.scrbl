#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Conditional Lenses}

@defmodule[unstable/lens/if]

@defproc[(lens-if [pred (-> target/c any/c)]
                  [lens1 (lens/c target/c view/c)]
                  [lens2 (lens/c target/c view/c)])
         (lens/c target/c view/c)]{
Creates a lens that uses @racket[lens1] when the target satisfies
@racket[pred], and uses @racket[lens2] when the target doesn't satisfy
@racket[pred].
@lens-unstable-examples[
  (define if-lens (lens-if list? first-lens (vector-ref-lens 0)))
  (lens-view if-lens '(1 2 3))
  (lens-view if-lens '#(1 2 3))
  (lens-set if-lens '(1 2 3) 'a)
  (lens-set if-lens '#(1 2 3) 'a)
]}

@defform*[#:literals (else)
          [(lens-cond [pred-expr lens-expr] ... [else else-lens-expr])
           (lens-cond [pred-expr lens-expr] ...)]]{
Like @racket[lens-if], but based on @racket[cond] instead of
@racket[if]. It creates a lens that uses the first lens if the target matches the first
predicate, the second lens if the target matches the second predicate, and so on.
@lens-unstable-examples[
  (define cond-lens (lens-cond [list? first-lens]
                               [vector? (vector-ref-lens 0)]
                               [string? (string-ref-lens 0)]))
  (lens-view cond-lens '(1 2 3))
  (lens-view cond-lens '#(1 2 3))
  (lens-view cond-lens "123")
  (lens-set cond-lens '(1 2 3) 'a)
  (lens-set cond-lens '#(1 2 3) 'a)
  (lens-set cond-lens "123" #\a)
]}

@defform[(lens-match [pat lens-expr] ...)]{
Like @racket[lens-if] and @racket[lens-cond], but based on pattern
matching the target against each @racket[pat] with @racket[match].
It creates a lens that uses the first lens if the target matches the
first @racket[pat], the second lens if it matches the second
@racket[pat], and so on.
@lens-unstable-examples[
  (define lens (lens-match [(list a) first-lens]
                           [(list a b) second-lens]))
  (lens-view lens '(1))
  (lens-view lens '(1 2))
  (lens-set lens '(1) 'a)
  (lens-set lens '(1 2) 'a)
]}
