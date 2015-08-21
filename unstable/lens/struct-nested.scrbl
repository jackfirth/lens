#lang scribble/manual

@(require lens/doc-util/main)

@title{Nested struct lenses}

@defmodule[unstable/lens/struct-nested]

@(define-persistant-lenses-unstable-examples struct-nested-examples)

@defform[#:id struct-nested-lens
         (struct-nested-lens struct-id intermediate ... field-id)
         #:grammar ([intermediate (code:line [subfield-id subfield-struct-id]) both-id])]{
  Constructs a lens that views nested structures. The first @racket[struct-id] is the
  outermost struct type, the last @racket[field-id] is the innermost target field. Each
  @racket[intermediate] specifies how to walk down one level of the nested structs, and
  is either a pair of a @racket[subfield-id] and that field's struct type in
  @racket[subfield-struct-id], or a single @racket[both-id] in the common case that the
  field name is the same as the struct name.

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
  This takes advantage of the fact that each struct's fields are the same name
  as the struct that that field is a value of.
  @struct-nested-examples[
    (define game-player-posn-x-lens
      (struct-nested-lens game player posn x))
    (lens-view game-player-posn-x-lens the-game)
    (lens-set game-player-posn-x-lens the-game 3)
  ]

  In the case of the player's combat stats, the field is @italic{not} the
  same name as the @racket[combat-stats] struct. Therefore, we use the
  more verbose @racket[[subfield-id subfield-struct-id]] form for that
  step of the nested struct traversal.
  @struct-nested-examples[
    (define game-player-health-lens
      (struct-nested-lens game player [stats combat-stats] health))
    (lens-view game-player-health-lens the-game)
    (lens-set game-player-health-lens the-game 20)
]}
