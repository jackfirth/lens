#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Lenses for nested data}

@defform[(define-nested-lenses [base-id base-lens-expr] clause ...)
         #:grammar ([clause [sub-id sub-lens-expr
                              clause
                              ...]])]{
A shorthand for defining composed lenses for nested data structures.

For example, if there is a @racket[top] struct containing a
@racket[middle] struct, which contains an @racket[x] field and a
@racket[y] field, a form like:
@(racketblock
  (define-nested-lenses [top-middle top-middle-lens]
    [x middle-x-lens]
    [y middle-y-lens]))
Will define @racket[top-middle-x-lens] and @racket[top-middle-y-lens]
as @racket[(lens-thrush top-middle-lens middle-x-lens)] and
@racket[(lens-thrush top-middle-lens middle-y-lens)].

Clauses can be nested within other clauses as well:

@lens-unstable-examples[
  (struct/lens game (player1 player2) #:transparent)
  (struct/lens player (position score) #:transparent)
  (struct/lens position (x y) #:transparent)
  (define-nested-lenses [game-player1 game-player1-lens]
    [score player-score-lens]
    [position player-position-lens
      [x position-x-lens]
      [y position-y-lens]])
  (define-nested-lenses [game-player2 game-player2-lens]
    [score player-score-lens]
    [position player-position-lens
      [x position-x-lens]
      [y position-y-lens]])
  (define the-game (game (player (position 1 2) 5) (player (position 3 4) 6)))
  (lens-view game-player1-score-lens the-game)
  (lens-view game-player1-position-lens the-game)
  (lens-view game-player1-position-x-lens the-game)
  (lens-set game-player1-score-lens the-game 9005)
  (lens-set game-player1-position-lens the-game (position 2 0))
  (lens-set game-player1-position-x-lens the-game 3)
]}
