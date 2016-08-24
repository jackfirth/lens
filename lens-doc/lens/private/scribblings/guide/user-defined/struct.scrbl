#lang scribble/manual

@(require scribble/eval
          lens/private/doc-util/main)

@(define make-lens-eval
   (make-eval-factory '(racket/base lens)))
@(define-syntax-rule (lens-interaction expr ...)
   (interaction #:eval (make-lens-eval) expr ...))

@(define struct-eval (make-lens-eval))
@(define-syntax-rule (struct-interaction expr ...)
   (interaction #:eval struct-eval expr ...))

@title[#:tag "struct-guide"]{Structures}

@see-reference-note["struct-reference"]{struct lenses}

Racket's structure system is an extremely useful tool to help model your problem domain, but using
them in a functional style can be laborious. To make this easier, @racketmodname[lens] provides helper
macros to automatically generate lenses for structure fields, which can be composed just like any
other lenses to allow easy functional programming with nested structs.

To start using lenses with structs, use the @racket[struct/lens] form when defining a structure
instead of @racket[struct]:

@(struct-interaction
  (struct/lens point (x y) #:transparent))

This will define a struct called @racket[point], and it will also produce two lenses,
@racket[point-x-lens] and @racket[point-y-lens]. It's also possible to define lenses for an
@emph{existing} structure type using @racket[define-struct-lenses].

@(lens-interaction
  (struct point (x y) #:transparent)
  (define-struct-lenses point))

If you don't want to use the auto-definition behavior of @racket[struct/lens] or
@racket[define-struct-lenses], you can also use @racket[struct-lens] to create one-off lenses for
particular fields.

@(lens-interaction
  (struct point (x y) #:transparent)
  (struct-lens point x))

One created, structure lenses work just like any other lenses: they can be used with functions like
@racket[lens-view], @racket[lens-set], and @racket[lens-transform], and they can be composed with
other lenses to produce new ones.

@(struct-interaction
  (lens-view point-x-lens (point 4 10))
  (lens-set point-y-lens (point 4 10) 15)
  (lens-transform point-x-lens (point 4 10)
                  (λ (x) (* x 3))))

Composition of struct lenses can make it much easier to write purely functional state transformations,
such as the “world programs” of @italic{How to Design Programs}. For example, given some state:

@(struct-interaction
  (struct/lens world (play-time player-stats) #:transparent)
  (struct/lens player-stats (health attack) #:transparent)
  (struct/lens monster (attack) #:transparent))

It's possible to write updater functions that manipulate nested state without completely destructuring
and rebuilding the structure each time:

@(struct-interaction
  (define (perform-monster-attack world monster)
    (lens-transform (lens-compose player-stats-health-lens
                                  world-player-stats-lens)
                    world
                    (λ (hp) (- hp (monster-attack monster)))))
  (let ([w (world 0 (player-stats 15 6))]
        [m (monster 2)])
    (perform-monster-attack w m)))
