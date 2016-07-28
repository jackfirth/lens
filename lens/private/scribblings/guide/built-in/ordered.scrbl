
#lang scribble/manual

@(require scribble/eval
          "../../../doc-util/main.rkt")

@(define make-lens-eval
   (make-eval-factory '(racket/base racket/function racket/list racket/stream lens)))

@(define-syntax-rule (lens-interaction expr ...)
   (interaction #:eval (make-lens-eval) expr ...))

@title[#:tag "ordered-data-lenses"]{Lenses on Ordered Data}

Many Racket data structures hold @emph{ordered} or @emph{sequential} values. Lenses for accessing
elements of these structures by index are provided.

@section[#:tag "pair-list-guide" #:style 'quiet]{Pairs and Lists}

@see-reference-note["pair-list-reference"]{pair and list lenses}

The two primitive pair lenses are @racket[car-lens] and @racket[cdr-lens]:

@(lens-interaction
  (lens-transform car-lens '(1 . 2) (curry * 2))
  (lens-transform cdr-lens '(1 . 2) (curry * 2)))

Obviously, these also work with lists, but most of the time, it's easier to use list-specific lenses.
For arbitrary access to elements within a list, use the @racket[list-ref-lens] lens constructor, which
produces a new lens given an index to look up. Abbreviation lenses such as @racket[first-lens] and
@racket[second-lens] are provided for common use-cases:

@(lens-interaction
  (lens-transform (list-ref-lens 3) (range 10) sub1)
  (lens-transform third-lens (range 10) sub1))

This is useful, but it only works for flat lists. However, using lens composition, it is possible to
create a lens that performs indexed lookups for nested lists using only @racket[list-ref-lens]:

@(lens-interaction
  (define (2d-list-ref-lens x y)
    (lens-compose (list-ref-lens x)
                  (list-ref-lens y)))
  (lens-set (2d-list-ref-lens 1 2)
            '((1 2 3)
              (4 5 6)
              (7 8 9))
            0))

This can also be generalized to @emph{n}-dimensional lists:

@(lens-interaction
  (define (list-ref-lens* . indices)
    (apply lens-compose (map list-ref-lens indices)))
  (lens-set (list-ref-lens* 0 1 0)
            '(((a b) (c d))
              ((e f) (g h)))
            'z))

This function is actually provided by @racketmodname[lens] under the name
@racket[list-ref-nested-lens], but the above example demonstrates that it's really a derived concept.

@subsection{Fetching multiple list values at once}

Sometimes it can be useful to fetch multiple values from a list with a single lens. This can be done
with @racket[lens-join/list], which combines multiple lenses whose target is a single value and
produces a new lens whose view is all of those values.

@(lens-interaction
  (define first-two-lens (lens-join/list first-lens second-lens))
  (lens-view first-two-lens '(1 2 3 4))
  (lens-set first-two-lens '(1 2 3 4) '(a b))
  (lens-transform first-two-lens '(1 2 3 4) (curry map sub1)))

This can be useful to implement a form of information hiding, in which only a portion of a list is
provided to client code, but the result can still be used to update the original list.

@section[#:tag "vectors-strings-guide"]{Vectors and Strings}

@other-reference-note{
  The @secref["vectors-reference"] and @secref["strings-reference"] sections in The Lens Reference
  have additional information on vector and string lenses, respectively.}

Lenses for random-access retrieval and functional update on vectors and strings are similar to the
lenses provided for lists, but unlike lists, they are truly random-access. The
@racket[vector-ref-lens] and @racket[string-ref-lens] lens constructors produce random-access lenses,
and @racket[lens-join/vector] and @racket[lens-join/string] combine multiple lenses with vector or
string targets.

@(lens-interaction
  (lens-transform (vector-ref-lens 1) #("a" "b" "c") string->symbol)
  (lens-transform (string-ref-lens 3) "Hello!" char-upcase))

@section[#:tag "streams-guide"]{Streams}

@see-reference-note["streams-reference"]{stream lenses}

Racket's @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{streams} contain ordered data,
much like lists, but unlike lists, they are @emph{lazy}. Lenses on streams are similarly lazy, only
forcing the stream up to what is necessary. This allows stream lenses to successfully operate on
infinite streams.

@(lens-interaction
  (lens-view (stream-ref-lens 10)
             (stream-map (curry expt 2) (in-naturals))))

Keep in mind that since @racket[lens-transform] is strict, using it to update a value within a stream
will force the stream up to the position of the element being modified.
