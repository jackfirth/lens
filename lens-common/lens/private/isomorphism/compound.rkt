#lang sweet-exp racket/base

require racket/contract/base
provide
  contract-out
    isomorphism-compose
      (rest-> isomorphism-lens? isomorphism-lens?)
    isomorphism-thrush
      (rest-> isomorphism-lens? isomorphism-lens?)
  isomorphism-cond
  isomorphism-join

require racket/match
        lens/private/util/rest-contract
        syntax/parse/define
        "base.rkt"
        for-syntax
          racket/base
module+ test
  require lens/private/base/main
          lens/private/compound/identity
          lens/private/isomorphism/data
          racket/function
          rackunit

;; ------------------------------------------------------------------------

(define (isomorphism-compose . args)
  (match args
    [(list (make-isomorphism-lens fs invs) ...)
     (make-isomorphism-lens
      (apply compose1 fs)
      (apply compose1 (reverse invs)))]))

(define (isomorphism-thrush . args)
  (apply isomorphism-compose (reverse args)))

;; -----------------------------------------------------------------------

(define-simple-macro
  (isomorphism-cond [pred-a? iso-a->b pred-b?] ...)
  #:with [a->b ...] (generate-temporaries #'[iso-a->b ...])
  #:with [b->a ...] (generate-temporaries #'[iso-a->b ...])
  (match-let ([(make-isomorphism-lens a->b b->a) iso-a->b] ...)
    (make-isomorphism-lens
     (λ (a)
       (cond [(pred-a? a) (check (a->b a) pred-b?)]
             ...
             [else
              (error 'isomorphism-cond
                     "all cond conditions were false\n  target: ~v"
                     a)]))
     (λ (b)
       (cond [(pred-b? b) (check (b->a b) pred-a?)]
             ...
             [else
              (error 'isomorphism-cond
                     "all cond conditions were false\n  view: ~v"
                     b)])))))

(define (check val pred?)
  (unless (pred? val)
    (error 'isomorphism-cond
           "promised a value passing ~v, given ~v"
           pred? val))
  val)

;; -----------------------------------------------------------------------

;; |---------------------------------------------------------------------|
;; | (isomorphism-join                                                   |
;; |   make-a                                                            |
;; |   make-b                                                            |
;; |   [a-selector  iso-a-piece->b-piece  b-selector]                    |
;; |   ...)                                                              |
;; | -> [Isomorphism A B]                                                |
;; | where:                                                              |
;; |   make-a     : A-Piece ... -> A                                     |
;; |   make-b     : B-Piece ... -> B                                     |
;; |   a-selector : A -> A-Piece                                         |
;; |   ...                                                               |
;; |   b-selector : B -> B-Piece                                         |
;; |   ...                                                               |
;; |   iso-a-piece->b-piece : [Isomorphism A-Piece B-Piece]              |
;; |   ...                                                               |
;; |---------------------------------------------------------------------|
;;
;; In order for this to be a valid isomorphism, each piece isomorphism must
;; be a valid isomorphism between the piece types, and these properties
;; must be true for all a, b, a-piece..., and b-piece...:
;; (make-a (a-selector a) ...) = a
;; (make-b (b-selector b) ...) = b
;; (a-selector (make-a a-piece ...)) = a-piece
;; ...
;; (b-selector (make-b b-piece ...)) = b-piece
;; ...

(define-simple-macro
  (isomorphism-join mk-a mk-b [a-sel iso-piece b-sel] ...)
  #:with [a-selector ...] (generate-temporaries #'[a-sel ...])
  #:with [b-selector ...] (generate-temporaries #'[b-sel ...])
  #:with [ap->bp ...] (generate-temporaries #'[iso-piece ...])
  #:with [bp->ap ...] (generate-temporaries #'[iso-piece ...])
  (match-let ([make-a mk-a]
              [make-b mk-b]
              [a-selector a-sel] ...
              [b-selector b-sel] ...
              [(make-isomorphism-lens ap->bp bp->ap) iso-piece] ...)
    (make-isomorphism-lens
     (λ (a)
       (make-b (ap->bp (a-selector a)) ...))
     (λ (b)
       (make-a (bp->ap (b-selector b)) ...)))))

;; What are (inv (f a)) and (f (inv b))?
;; Context:
;; C1. (make-a (a-selector a) ...) = a
;; C2. (make-b (b-selector b) ...) = b
;; C3. (a-selector (make-a a-piece ...)) = a-piece
;;     ...
;; C4. (b-selector (make-b b-piece ...)) = b-piece
;;     ...
;; C5. (b-piece->a-piece (a-piece->b-piece a-piece)) = a-piece
;;     ...
;; C6. (a-piece->b-piece (b-piece->a-piece b-piece)) = b-piece
;;     ...

;; Proof for (inv (f a)) = a:
;;   (inv (f a))
;; =   {Def. f}
;;   (inv (make-b (a-piece->b-piece (a-selector a)) ...))
;; =   {Def. inv}
;;   let b = (make-b (a-piece->b-piece (a-selector a)) ...)
;;   in (make-a (b-piece->a-piece (b-selector b)) ...)
;; =   {C4}
;;   (make-a (b-piece->a-piece (a-piece->b-piece (a-selector a))) ...)
;; =   {C5}
;;   (make-a (a-selector a) ...)
;; =   {C1}
;;   a

;; Proof for (f (inv b)) = b:
;;   (f (inv b))
;; =   {Def. inv}
;;   (f (make-a (b-piece->a-piece (b-selector b)) ...))
;; =   {Def. f}
;;   let a = (make-a (b-piece->a-piece (b-selector b)) ...)
;;   in (make-b (a-piece->b-piece (a-selector a)) ...)
;; =   {C3}
;;   (make-b (a-piece->b-piece (b-piece->a-piece (b-selector b))) ...)
;; =   {C6}
;;   (make-b (b-selector b) ...)
;; =   {C2}
;;   b

;; -----------------------------------------------------------------------

module+ test
  test-case "compose and thrush"
    (define string->vector-lens
      (isomorphism-thrush string->list-lens list->vector-lens))
    (check-equal? (lens-view string->vector-lens "abc") #(#\a #\b #\c))
    (check-equal? (lens-set string->vector-lens "abc" #(#\1 #\2 #\3)) "123")
    ;
    (define char-vector->string-lens
      (isomorphism-compose list->string-lens vector->list-lens))
    (check-equal? (lens-view char-vector->string-lens #(#\a #\b #\c)) "abc")
    (check-equal? (lens-set char-vector->string-lens #(#\a #\b #\c) "123")
                  #(#\1 #\2 #\3))
  ;
  test-case "cond and join"
    (struct nat [n] #:transparent)
    (struct neg-sub1 [n] #:transparent)
    ;; An Int is one of:
    ;;  - (nat Natural)
    ;;  - (neg-sub1 Natural)
    (define int->integer-lens
      (isomorphism-cond
        [nat?
         (isomorphism-join
           nat
           identity
           [nat-n  identity-lens  identity])
         (negate negative?)]
        [neg-sub1?
         (isomorphism-join
           neg-sub1
           (λ (x) (sub1 (- x)))
           [neg-sub1-n  identity-lens  (λ (n) (- (add1 n)))])
         negative?]))
    (check-equal? (lens-view int->integer-lens (nat 74)) 74)
    (check-equal? (lens-view int->integer-lens (neg-sub1 35)) -36)
    (check-equal? (lens-set int->integer-lens (nat 74) 9) (nat 9))
    (check-equal? (lens-set int->integer-lens (nat 74) -36) (neg-sub1 35))
    (check-equal? (lens-set int->integer-lens (neg-sub1 35) 9) (nat 9))
    (check-equal? (lens-set int->integer-lens (neg-sub1 35) -20) (neg-sub1 19))
  ;
  test-case "cond error"
    (define dom-num-str-lens
      (isomorphism-cond
        [number?  identity-lens  number?]
        [string?  identity-lens  string?]))
    (check-equal? (lens-view dom-num-str-lens "Immastring") "Immastring")
    (check-equal? (lens-view dom-num-str-lens 12345) 12345)
    (check-equal? (lens-set dom-num-str-lens 12345 "replacement") "replacement")
    ;
    (check-exn
      #rx"isomorphism-cond: all cond conditions were false\n  target: '\\(23\\)"
      (λ ()
        (lens-view dom-num-str-lens (list 23))))
    (check-exn
      #rx"isomorphism-cond: all cond conditions were false\n  view: '\\(23\\)"
      (λ ()
        (lens-set dom-num-str-lens 12345 (list 23))))
    ;
    (define error-lens
      (isomorphism-cond
        [number?  identity-lens  string?]))
    (check-exn
      #rx"promised a value passing #<procedure:string\\?>, given 4"
      (λ ()
        (lens-view error-lens 4)))
    (check-exn
      #rx"promised a value passing #<procedure:number\\?>, given \"astri\""
      (λ ()
        (lens-set error-lens 4 "astri")))

