#lang scribble/manual

@(require "../doc-util/main.rkt")

@title{Lens Construction}

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
  Given a lens and a target, constructs the @italic{view} and the
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
