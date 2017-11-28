#lang sweet-exp racket/base

require racket/list
        racket/contract
        lens/private/base/main
        lens/private/util/alternating-list
        lens/private/util/rest-contract

module+ test
  require rackunit
          "../list/list-ref-take-drop.rkt"
          lens/private/test-util/test-lens

provide
  contract-out
    lens-join/list (rest-> lens? (lens/c any/c list?))

;; The joined lens only follows the lens laws if the views of the argument
;; lenses don't overlap.

(define (lens-join/list . lenses)
  (define (get target)
    (apply lens-view/list target lenses))
  (define (set target new-views)
    (apply lens-set/list target (keys+values->alternating-list lenses new-views)))
  (make-lens get set))

;; joined-lens = (lens-join/list X-piece-lens ...)

;; For joined-lens to follow the lens laws, these properties have to hold for
;; all X and X-pieces:
;; (joined-set X (joined-get X)) = X
;; (joined-get (joined-set X X-pieces)) = X-pieces
;; where:
;; X-pieces = (list X-piece ...)
;; and
;; each X-piece is a valid view for X-piece-lens

;; If the views of the X-piece-lenses don't overlap, then for every X-piece...:
;; (lens-view X-piece-lens (lens-set/list X {X-piece-lens X-piece} ...))
;; =
;; X-piece
;; ...

;; Context:
;; C1:   {lens laws for X-piece-lens}
;;     (lens-set X-piece-lens X (lens-view X-piece-lens X)) = X
;;     ...
;; C2:   {lens laws for X-piece-lens}
;;     (lens-view X-piece-lens (lens-set X X-piece-lens X-piece)) = X-piece
;;     ...
;; C3:   {Def. lens-set/list, repeated application of C1}
;;     (lens-set/list X {X-piece-lens (lens-view X-piece-lens X)} ...) = X
;; C4:   {assumption that X-piece-lenses don't overlap}
;;     (lens-view X-piece-lens (lens-set/list X {X-piece-lens X-piece} ...))
;;     =
;;     X-piece
;;     ...

;; Proof for (joined-set X (joined-get X)) = X:
;;   (joined-set X (joined-get X))
;; =   {Def. joined-get}
;;   (joined-set X (lens-view/list X X-piece-lens ...))
;; =   {Def. lens-view/list}
;;   (joined-set X (list (lens-view X-piece-lens X) ...))
;; =   {Def. joined-set}
;;   (lens-set/list X {X-piece-lens (lens-view X-piece-lens X)} ...)
;; =   {C3}
;;   X

;; Proof for (joined-get (joined-set X X-pieces)) = X-pieces:
;;   (joined-get (joined-set X X-pieces))
;; =   {Def. joined-set, Def. X-pieces}
;;   (joined-get (lens-set/list X {X-piece-lens X-piece} ...))
;; =   {Def. joined-get}
;;   let Y = (lens-set/list X {X-piece-lens X-piece} ...)
;;   in (lens-view/list Y X-piece-lens ...)
;; =   {Def. lens-view/list}
;;   let Y = (lens-set/list X {X-piece-lens X-piece} ...)
;;   in (list
;;       (lens-view X-piece-lens Y)
;;       ...)
;; =   {C4}
;;   (list X-piece ...)
;; =   {Def. X-pieces}
;;   X-pieces

(module+ test
  (define first-third-fifth-lens
    (lens-join/list first-lens
                    third-lens
                    fifth-lens))
  (check-lens-view first-third-fifth-lens '(a b c d e f)
                   '(a c e))
  (check-lens-set first-third-fifth-lens '(a b c d e f) '(1 2 3)
                  '(1 b 2 d 3 f)))
