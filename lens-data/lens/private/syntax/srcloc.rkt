#lang sweet-exp racket/base

provide syntax-srcloc-lens
        syntax-source-lens
        syntax-line-lens
        syntax-position-lens
        syntax-column-lens
        syntax-span-lens

require fancy-app
        lens/common
        syntax/srcloc
module+ test
  require rackunit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntax

;; syntax-srcloc : Syntax -> Srcloc
(define (syntax-srcloc stx)
  (source-location->srcloc stx))

;; syntax-set-source-location : Syntax Source-Location -> Syntax
(define (syntax-set-source-location stx src)
  (datum->syntax stx
                 (syntax-e stx)
                 (source-location->list src)
                 stx))

(define syntax-srcloc-lens
  (make-lens
   syntax-srcloc
   syntax-set-source-location))

(define syntax-source-lens
  (make-lens
   syntax-source
   (update-source-location _ #:source _)))

(define syntax-line-lens
  (make-lens
   syntax-line
   (update-source-location _ #:line _)))

(define syntax-column-lens
  (make-lens
   syntax-column
   (update-source-location _ #:column _)))

(define syntax-position-lens
  (make-lens
   syntax-position
   (update-source-location _ #:position _)))

(define syntax-span-lens
  (make-lens
   syntax-span
   (update-source-location _ #:span _)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Source Locations

;; source-location->srcloc : Source-Location -> Srcloc
(define (source-location->srcloc src)
  (build-source-location src))

;; source-location->list : Source-Location -> Source-Location-List
(define (source-location->list src)
  (build-source-location-list src))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tests

(module+ test
  (define-check (check-syntax actual-stx expected-datum expected-srcloc)
    (check-pred syntax? actual-stx)
    (check-equal? (syntax->datum actual-stx) expected-datum)
    (check-equal? (syntax-srcloc actual-stx) expected-srcloc))

  (define a-src (srcloc 'a 12 5 144 9))
  (define b-src (srcloc 'b 49 7 343 14))
  (define a (datum->syntax #f (list '+ 1 2 3) (source-location->list a-src)))
  (define b (datum->syntax #f (list 'define 'x 987) (source-location->list b-src)))

  (test-case "syntax-srcloc-lens"
    (check-equal? (lens-view syntax-srcloc-lens a) a-src)
    (check-equal? (lens-view syntax-srcloc-lens b) b-src)
    (check-syntax (lens-set syntax-srcloc-lens a a-src) (list '+ 1 2 3) a-src)
    (check-syntax (lens-set syntax-srcloc-lens b b-src) (list 'define 'x 987) b-src)
    (check-syntax (lens-set syntax-srcloc-lens a b-src) (list '+ 1 2 3) b-src)
    (check-syntax (lens-set syntax-srcloc-lens b a-src) (list 'define 'x 987) a-src)
    )
  (test-case "syntax-source-lens"
    (check-equal? (lens-view syntax-source-lens a) 'a)
    (check-equal? (lens-view syntax-source-lens b) 'b)
    (check-syntax (lens-set syntax-source-lens a "bye.rkt")
                  (list '+ 1 2 3)
                  (srcloc "bye.rkt" 12 5 144 9))
    (check-syntax (lens-set syntax-source-lens b "hellooo.rkt")
                  (list 'define 'x 987)
                  (srcloc "hellooo.rkt" 49 7 343 14))
    )
  (test-case "syntax-line-lens"
    (check-equal? (lens-view syntax-line-lens a) 12)
    (check-equal? (lens-view syntax-line-lens b) 49)
    (check-syntax (lens-set syntax-line-lens a 8)
                  (list '+ 1 2 3)
                  (srcloc 'a 8 5 144 9))
    (check-syntax (lens-set syntax-line-lens b 11)
                  (list 'define 'x 987)
                  (srcloc 'b 11 7 343 14))
    )
  (test-case "syntax-column-lens"
    (check-equal? (lens-view syntax-column-lens a) 5)
    (check-equal? (lens-view syntax-column-lens b) 7)
    (check-syntax (lens-set syntax-column-lens a 8)
                  (list '+ 1 2 3)
                  (srcloc 'a 12 8 144 9))
    (check-syntax (lens-set syntax-column-lens b 11)
                  (list 'define 'x 987)
                  (srcloc 'b 49 11 343 14))
    )
  (test-case "syntax-position-lens"
    (check-equal? (lens-view syntax-position-lens a) 144)
    (check-equal? (lens-view syntax-position-lens b) 343)
    (check-syntax (lens-set syntax-position-lens a 233)
                  (list '+ 1 2 3)
                  (srcloc 'a 12 5 233 9))
    (check-syntax (lens-set syntax-position-lens b 610)
                  (list 'define 'x 987)
                  (srcloc 'b 49 7 610 14))
    )
  (test-case "syntax-span-lens"
    (check-equal? (lens-view syntax-span-lens a) 9)
    (check-equal? (lens-view syntax-span-lens b) 14)
    (check-syntax (lens-set syntax-span-lens a 10)
                  (list '+ 1 2 3)
                  (srcloc 'a 12 5 144 10))
    (check-syntax (lens-set syntax-span-lens b 15)
                  (list 'define 'x 987)
                  (srcloc 'b 49 7 343 15))
    )
  )
