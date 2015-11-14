#lang scribble/manual

@(require scribble/eval
          "../../doc-util/main.rkt")

@(define introduction-eval ((make-eval-factory '(racket/base lens))))

@title[#:tag "lens-intro"]{Introduction to Lenses}

The @racketmodname[lens] library defines @lens-tech{lenses}, tools for extracting values from
potentially-nested data structures. Lenses are most useful when writing in a functional style, such as
the style employed by @italic{How to Design Programs}, in which data structures are immutable and
side-effects are kept to a minimum.

@section{What are lenses?}

A @deftech[#:key "lens" #:normalize? #f]{lens} is a value that composes a getter and a setter function
to produce a bidirectional view into a data structure. This definition is intentionally broad---lenses
are a very general concept, and they can be applied to almost any kind of value that encapsulates
data. To make the concept more concrete, consider one of Racket's most primitive datatypes, the
@tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{pair}. Pairs are constructed from two values using
the @racket[cons] function; the first value can then be retrieved using @racket[car], and the second
can be retrieved using @racket[cdr].

@(interaction
  (define p (cons 1 2))
  p
  (car p)
  (cdr p))

With these three primitives, it's very easy to create new pairs and subsequently extract values from
them. However, it's a little bit harder to update a single field in an existing pair. In a traditional
Scheme, this could be accomplished by using @racket[set-car!] or @racket[set-cdr!], but these
@emph{mutate} the original pair. To remain functional, we want to produce an @emph{entirely new} pair
with one of the fields updated.

Fortunately, this is quite easy to implement in Racket:

@(interaction #:eval introduction-eval
  (define (set-car p v)
    (cons v (cdr p)))
  (define (set-cdr p v)
    (cons (car p) v))
  (set-car (cons 1 2) 'x)
  (set-cdr (cons 1 2) 'y))

@other-reference-note{
 Both @racket[car-lens] and @racket[cdr-lens], are provided by @racketmodname[lens] out of the box,
 along with some other shorthand lenses. For the full list, see @secref{pair-lenses}.}

This means that each field now has a pair of getters and setters: @racket[car]/@racket[set-car] and
@racket[cdr]/@racket[set-cdr]. A lens just wraps up each of these pairs of functions into a single
value, so instead of having four functions, we would just have two lenses: @racket[car-lens] and
@racket[cdr-lens]. In fact, using the functions we've just written, we can implement these lenses
ourselves.

@(interaction #:eval introduction-eval
  (define car-lens (make-lens car set-car))
  (define cdr-lens (make-lens cdr set-cdr)))

To use a lens's getter function, use @racket[lens-view]. To use the setter function, use
@racket[lens-set]:

@(interaction #:eval introduction-eval
  (lens-view car-lens (cons 1 2))
  (lens-set car-lens (cons 1 2) 'x))

This, of course, isn't very useful, since we could just use the functions on their own. One extra
thing we @emph{do} get for free when using lenses is @racket[lens-transform]. This allows you to
provide a procedure which will update the “view” based on its existing value. For example, we could
increment one element in a pair:

@(interaction #:eval introduction-eval
  (lens-transform cdr-lens (cons 1 2) add1))

While that's kind of cool, it still probably isn't enough to justify using lenses instead of just
using functions.

@section[#:style 'quiet]{Why use lenses?}

So far, lenses just seem like a way to group getters and setters, and as we've seen, that's really all
they are. However, on their own, this wouldn't be very useful. Using @racket[(car _p)] is a lot easier
than using @racket[(lens-view car-lens _p)].

Using plain functions starts to get a lot harder, though, once you start nesting data structures. For
example, consider a tree constructed by nesting pairs inside of pairs:

@(interaction #:eval introduction-eval
  (define tree (cons (cons 'a 'b)
                     (cons 'c 'd))))

Now, getting at a nested value gets much harder. It's necessary to nest calls to get at the right
value:

@(interaction #:eval introduction-eval
  (cdr (car tree)))

Still, this isn't too bad. However, what if we want to @emph{set} a value? We could use our
@racket[set-car] and @racket[set-cdr] functions from earlier, but if we try, we'll find they don't
work quite right:

@(interaction #:eval introduction-eval
  (set-cdr (car tree) 'x))

Oops. We wanted to get back the whole tree, but we just got back one of the internal nodes because
we used @racket[set-cdr] on that node. In order to actually do what we want, we'd need to add a lot
more complexity:

@(interaction #:eval introduction-eval
  (set-car tree (set-cdr (car tree) 'x)))

That's what we need to do just for @emph{one} level of nesting---it would be much worse for any more
than that. How can we solve it?

@subsection{Lens composition}

@other-reference-note{For more ways to construct compound lenses, see @secref{composing-lenses}.}

In order to solve this problem, we can use @emph{lens composition}, which is similar to function
composition but extended to lenses. Just as we can create a compound getter function with the
expression @racket[(compose cdr car)], we can create a compound lens with the expression
@racket[(lens-compose cdr-lens car-lens)]. With this, we produce an entirely new lens that can be used
with @racket[lens-view], @racket[lens-set], and @racket[lens-transform], all of which do what you
would expect:

@(interaction #:eval introduction-eval
  (define cdar-lens (lens-compose cdr-lens car-lens))
  (lens-view cdar-lens tree)
  (lens-set cdar-lens tree 'x)
  (lens-transform cdar-lens tree symbol->string))

Now the reason lenses are useful may begin to crystallize: they make it possible to not just get at
but to actually functionally update and transform values within deeply-nested data structures. Since
they are composable, it is easy to create lenses that can traverse any set of structures with nothing
but a small set of primitives. This library provides those primitives.
