#lang scribble/manual

@(require lens/private/doc-util/main (for-label syntax/stx))

@title{Syntax object lenses based on @racketmodname[syntax/stx]}

@defthing[stx->list-lens lens?]{
A lens that views a stx-list as a list. Viewing with this lens is
equivalent to using @racket[stx->list], and if the target is a syntax
object, setting it with this lens preserves the lexical context,
source location, and syntax properties of the outer syntax object.

@lens-unstable-examples[
  (lens-view stx->list-lens #'(a b c))
  (lens-set stx->list-lens #'(a b c) '(1 2 3))
]}

@defproc[(stx-map-lens [lens lens?]) lens?]{
Creates a lens that maps @racket[lens] over a target stx-list. Like
@racket[stx->list-lens], setting with a syntax object target preserves
lexical context, location, and properties.

This is the syntax version of @racket[map-lens]. 

@lens-unstable-examples[
  (lens-view (stx-map-lens stx-car-lens) #'((a b) (c d) (e f)))
  (lens-set (stx-map-lens stx-car-lens) #'((a b) (c d) (e f)) #'(1 2 3))
]}

@deftogether[[
  @defthing[stx-car-lens lens?]
  @defthing[stx-cdr-lens lens?]
]]{
Lenses for looking at the car and cdr of syntax-pairs.

These are the syntax versions of @racket[car-lens] and @racket[cdr-lens].

@lens-unstable-examples[
  (lens-view stx-car-lens #'(a . b))
  (lens-view stx-cdr-lens #'(a . b))
  (lens-set stx-car-lens #'(a . b) #'1)
  (lens-set stx-cdr-lens #'(a . b) #'1)
]}

@deftogether[[
  @defthing[stx-caar-lens lens?]
  @defthing[stx-cdar-lens lens?]
  @defthing[stx-cadr-lens lens?]
  @defthing[stx-cddr-lens lens?]
  @defthing[stx-caaar-lens lens?]
  @defthing[stx-cdaar-lens lens?]
  @defthing[stx-cadar-lens lens?]
  @defthing[stx-cddar-lens lens?]
  @defthing[stx-caadr-lens lens?]
  @defthing[stx-cdadr-lens lens?]
  @defthing[stx-caddr-lens lens?]
  @defthing[stx-cdddr-lens lens?]
]]{
Lenses for accessing nested syntax-pairs.

@lens-unstable-examples[
  (lens-view stx-caddr-lens #'(a b c d))
  (lens-set stx-caddr-lens #'(a b c d) #'1)
]}

@defthing[stx-append*-lens lens?]{
A lens like that flattens a stx-list one-level down when viewing, and
restores the original structure when setting.

This is the syntax version of @racket[append*-lens].

@lens-unstable-examples[
  (lens-view stx-append*-lens #'((a) (b c) () (d e f)))
  (lens-set stx-append*-lens #'((a) (b c) () (d e f)) #'(1 2 3 4 5 6))
]}

@defproc[(stx-flatten/depth-lens [n exact-nonnegative-integer?]) lens?]{
Creates a lens that flattens a stx-list of depth @racket[n] when
viewing, and restores the original structure when setting.

This is the syntax version of @racket[flatten/depth-lens].

@lens-unstable-examples[
  (lens-view (stx-flatten/depth-lens 0) #'42)
  (lens-set (stx-flatten/depth-lens 0) #'42 #'(43))
  (lens-view (stx-flatten/depth-lens 1) #'(a b c))
  (lens-set (stx-flatten/depth-lens 1) #'(a b c) #'(1 2 3))
  (lens-view (stx-flatten/depth-lens 2) #'((a) (b c) () (d e f)))
  (lens-set (stx-flatten/depth-lens 2) #'((a) (b c) () (d e f)) #'(1 2 3 4 5 6))
  (lens-view (stx-flatten/depth-lens 3) #'(((a) ()) (() (b) (c)) () ((d e) () (f))))
  (lens-set (stx-flatten/depth-lens 3) #'(((a) ()) (() (b) (c)) () ((d e) () (f))) #'(1 2 3 4 5 6))
]}

@defproc[(stx-append*n-lens [n exact-nonnegative-integer?]) lens?]{
This is deprecated. Use @racket[(stx-flatten/depth-lens (add1 n))] instead.
}

