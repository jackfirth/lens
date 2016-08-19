#lang scribble/manual

@(require "../doc-util/main.rkt")

@title{Flattening and unflattening lists}

@defthing[append*-lens lens?]{
A lens that flattens a list one-level down when viewing, and restores
the original structure when setting. The target list must be a nested
list at least 2 levels deep, or a list of lists. Viewing with this
lens is equivalent to using @racket[append*], and setting with this
lens restores the structure of the original nested list.

When setting, the new view must have the same length as the old view,
so it must have the same length as @racket[(append* target)].

This is equivalent to @racket[(flatten/depth-lens 2)], since it
flattens lists of depth 2.
@lens-unstable-examples[
  (lens-view append*-lens '((a b c) (1 2 3)))
  (lens-set append*-lens '((a b c) (1 2 3)) '("do" "re" "mi" "re" "mi" "do"))
  (lens-view append*-lens '((a) (b c) () (d e f)))
  (lens-set append*-lens '((a) (b c) () (d e f)) '(1 2 3 4 5 6))
]
The further nested elements don't have to be atomic, they could be
other lists. @racket[append*-lens] doesn't recur into those.
@lens-unstable-examples[
  (lens-view append*-lens '(((a) (b) (c)) ((1) (2) (3))))
  (lens-set append*-lens '(((a) (b) (c)) ((1) (2) (3))) '("mi" "re" "do" "re" "re" "mi"))
]}

@defproc[(flatten/depth-lens [n exact-nonnegative-integer?]) lens?]{
Creates a lens that flattens a list of depth @racket[n] when
viewing, and restores the original structure when setting.

A list of depth @racket[0] is a single element, a list of depth
@racket[1] is a list, a list of depth @racket[2] is a list of lists,
and so on.

This is a generalization of @racket[append*-lens], with that being
equivalent to @racket[(flatten/depth-lens 2)]. It uses
@racket[flatten/depth] to view, and @racket[unflatten/depth] to set.

When setting, the new view must have the same length as the old view,
so it must have the same length as @racket[(flatten/depth n target)].
@lens-unstable-examples[
  (lens-view (flatten/depth-lens 0) 'a)
  (lens-set (flatten/depth-lens 0) 'a '(1))
  (lens-view (flatten/depth-lens 1) '(a b c))
  (lens-set (flatten/depth-lens 1) '(a b c) '(1 2 3))
  (lens-view (flatten/depth-lens 2) '((a) (b c) () (d e f)))
  (lens-set (flatten/depth-lens 2) '((a) (b c) () (d e f)) '(1 2 3 4 5 6))
  (lens-view (flatten/depth-lens 3) '(((a) ()) (() (b) (c)) () ((d e) () (f))))
  (lens-set (flatten/depth-lens 3) '(((a) ()) (() (b) (c)) () ((d e) () (f))) '(1 2 3 4 5 6))
]}

@defproc[(flatten/depth [n exact-nonnegative-integer?] [structure any/c]) list?]{
Flattens a list of depth @racket[n]. For depth @racket[n] = @racket[0],
it returns @racket[(list structure)]. For a depth of @racket[1], it
returns @racket[structure]. For a depth of @racket[2], it returns
@racket[(append* structure)]. For a depth of @racket[3], it returns
@racket[(append* (append* structure))], and so on for higher depths.

This is what @racket[flatten/depth-lens] uses for viewing.
@lens-unstable-examples[
  (flatten/depth 0 'a)
  (flatten/depth 1 '(a b c))
  (flatten/depth 2 '((a) (b c) () (d e f)))
  (flatten/depth 3 '(((a) ()) (() (b) (c)) () ((d e) () (f))))
]}

@defproc[(unflatten/depth [n exact-nonnegative-integer?] [structure any/c] [flattened list?]) any/c]{
Un-does the work done by @racket[flatten/depth], to return an
un-flattened version of @racket[flattened], with the structure restored based
on @racket[structure].

This is what @racket[flatten/depth-lens] uses for setting.
@lens-unstable-examples[
  (unflatten/depth 0 'a '(1))
  (unflatten/depth 1 '(a b c) '(1 2 3))
  (unflatten/depth 2 '((a) (b c) () (d e f)) '(1 2 3 4 5 6))
  (unflatten/depth 3 '(((a) ()) (() (b) (c)) () ((d e) () (f))) '(1 2 3 4 5 6))
]}

@defproc[(append*n-lens [n exact-nonnegative-integer?]) lens?]{
This is deprecated. Use @racket[(flatten/depth-lens (add1 n))] instead.
}

