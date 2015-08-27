#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Nested struct lenses}

@defmodule[unstable/lens/struct-nested]

@(persistent-lens-unstable-examples struct-nested-examples)

@defform[#:id struct-nested-lens
         (struct-nested-lens [struct-id field-id] ...)]{
  Constructs a lens that views nested structures. Each @racket[struct-id] and
  @racket[field-id] pair is paired into a lens for viewing that field of that
  struct, then the list of lenses are @racket[lens-thrush]ed together.

  For example, given a complicated nested tree of state representing a game:
  @struct-nested-examples[
    (struct game (player level) #:transparent)
    (struct player (posn stats) #:transparent)
    (struct posn (x y) #:transparent)
    (struct combat-stats (health attack) #:transparent)
    (define the-game (game (player (posn 0 0) (combat-stats 10 1)) 'foo-level))
    the-game
  ]

  We can create a lens for traversing the nested structures of the game state.
  At each step, we provide the name of the struct we're examining and the name
  of the field we wish to traverse into.
  @struct-nested-examples[
    (define game-player-health-lens
      (struct-nested-lens [game player]
                          [player stats]
                          [combat-stats health]))
    (lens-view game-player-health-lens the-game)
    (lens-set game-player-health-lens the-game 20)
]}

@(persistent-lens-unstable-examples struct-nested*-examples)

@defform[#:id struct-nested-lens*
         (struct-nested-lens* struct-id both-id ... field-id)]{
  Like @racket[struct-nested-lens], but for the case where each nested
  field is named the same as its struct type. For example, given the
  game state defined in the examples for @racket[struct-nested-lens]:
  @struct-nested*-examples[
    (struct game (player level) #:transparent)
    (struct player (posn stats) #:transparent)
    (struct posn (x y) #:transparent)
    (struct combat-stats (health attack) #:transparent)
    (define the-game (game (player (posn 0 0) (combat-stats 10 1)) 'foo-level))
    the-game
  ]

  Because each field is named the same as its struct type, we can
  create a lens for viewing the player's x coordinate more succinctly
  than with @racket[struct-nested-examples]:
  @struct-nested*-examples[
    (define game-player-x-lens
      (struct-nested-lens* game player posn x))
    (lens-view game-player-x-lens the-game)
    (lens-set game-player-x-lens the-game 5)
]}
