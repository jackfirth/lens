#lang sweet-exp racket/base

require racket/contract/base
provide
  contract-out
    lens-zoom (-> lens? lens? lens?)
    lens-zoom* (->* [] #:rest (listof2 lens? lens?) lens?)

require fancy-app
        lens/private/base/main
        lens/private/compound/thrush
        lens/private/util/list-pair-contract
        racket/match
        racket/sequence
        lens/private/isomorphism/base

;; lens-zoom : (Lens (Outer Inner) Inner) (Lens A B) -> (Lens (Outer A) (Outer B))
(define (lens-zoom zoom-lens transformer-lens)
  (match transformer-lens
    [(make-isomorphism-lens transformer inverse)
     ;; transformer : A -> B
     ;; inverse     : B -> A
     (make-isomorphism-lens
      (lens-transform zoom-lens _ transformer) ; (Outer A) -> (Outer B)
      (lens-transform zoom-lens _ inverse))]   ; (Outer B) -> (Outer A)
    [transformer-lens
     ;; get : (Outer A) -> (Outer B)
     (define (get tgt)
       ;; transformer : A -> B
       (define (transformer a)
         (lens-view transformer-lens a))
       (lens-transform zoom-lens tgt transformer))
     ;; set : (Outer A) (Outer B) -> (Outer A)
     (define (set tgt nvw)
       ;; a : A
       (define a (lens-view zoom-lens tgt))
       ;; transformer : B -> A
       (define (transformer b)
         (lens-set transformer-lens a b))
       (lens-transform zoom-lens nvw transformer))
     (make-lens get set)]))

(define (lens-zoom* . lenses/transformers)
  (apply lens-thrush
         (for/list ([args (in-slice 2 lenses/transformers)])
           (apply lens-zoom args))))
