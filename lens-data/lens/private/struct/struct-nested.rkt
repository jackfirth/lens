#lang racket/base

(require fancy-app
         lens
         (for-syntax racket/base
                     syntax/parse))

(module+ test
  (require rackunit))

(provide struct-nested-lens
         struct-nested-lens*)


(define-syntax struct-nested-lens
  (syntax-parser
    [(_ [struct-id:id field-id:id] ...)
     #'(lens-thrush (struct-lens struct-id field-id) ...)]))

(define-syntax struct-nested-lens*
  (syntax-parser
    [(_ struct-id:id field-id:id)
     #'(struct-lens struct-id field-id)]
    [(_ struct-id:id both0:id both:id ... field-id:id)
     #'(lens-thrush (struct-lens struct-id both0)
                    (struct-nested-lens* both0 both ... field-id))]))

(module+ test
  (struct game (player level) #:transparent)
  (struct player (posn stats) #:transparent)
  (struct posn (x y) #:transparent)
  (struct combat-stats (health attack) #:transparent)
  (define the-game (game (player (posn 0 0) (combat-stats 10 1)) 'foo-level))

  (define game-player-health-lens
    (struct-nested-lens [game player]
                        [player stats]
                        [combat-stats health]))
  (check-equal? (lens-view game-player-health-lens the-game) 10)
  (check-equal? (lens-set game-player-health-lens the-game 20)
                (game (player (posn 0 0) (combat-stats 20 1)) 'foo-level))
  
  (define game-player-posn-x-lens
    (struct-nested-lens* game player posn x))
  (check-equal? (lens-view game-player-posn-x-lens the-game) 0)
  (check-equal? (lens-set game-player-posn-x-lens the-game 3)
                (game (player (posn 3 0) (combat-stats 10 1)) 'foo-level)))

 