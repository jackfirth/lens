#lang sweet-exp racket/base

provide syntax-srcloc-lens
        syntax-source-lens
        syntax-line-lens
        syntax-position-lens
        syntax-column-lens
        syntax-span-lens
        source-location->srcloc-lens
        source-location->list-lens
        source-location->vector-lens
        source-location-source-lens
        source-location-line-lens
        source-location-column-lens
        source-location-position-lens
        source-location-span-lens

require fancy-app
        lens/common
        syntax/parse/define
        syntax/srcloc
module+ test
  require rackunit

(define-simple-macro
  (define-source-location-lenses [lens-id:id getter:expr update-kw:keyword] ...)
  (begin
    (define lens-id
      (make-lens getter (update-source-location _ update-kw _)))
    ...))

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

(define-source-location-lenses
  [syntax-source-lens   syntax-source   #:source]
  [syntax-line-lens     syntax-line     #:line]
  [syntax-column-lens   syntax-column   #:column]
  [syntax-position-lens syntax-position #:position]
  [syntax-span-lens     syntax-span     #:span])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Source Locations

;; source-location->srcloc : Source-Location -> Srcloc
(define (source-location->srcloc src)
  (build-source-location src))

;; source-location->list : Source-Location -> Source-Location-List
(define (source-location->list src)
  (build-source-location-list src))

;; source-location->vector : Source-Location -> Source-Location-Vector
(define (source-location->vector src)
  (build-source-location-vector src))

;; replace-source-location : Syntax Source-Location -> Syntax
;;                           Srcloc Source-Location -> Srcloc
;;                           Source-Location-List Source-Location -> Source-Location-List
;;                           Source-Location-Vector Source-Location -> Source-Location-Vector
;;                           Source-Location Source-Location -> Source-Location
(define (replace-source-location old new)
  (update-source-location old
    #:source (source-location-source new)
    #:line (source-location-line new)
    #:column (source-location-column new)
    #:position (source-location-position new)
    #:span (source-location-span new)))

(define source-location->srcloc-lens
  (make-lens
   source-location->srcloc
   replace-source-location))

(define source-location->list-lens
  (make-lens
   source-location->list
   replace-source-location))

(define source-location->vector-lens
  (make-lens
   source-location->vector
   replace-source-location))

(define-source-location-lenses
  [source-location-source-lens   source-location-source   #:source]
  [source-location-line-lens     source-location-line     #:line]
  [source-location-column-lens   source-location-column   #:column]
  [source-location-position-lens source-location-position #:position]
  [source-location-span-lens     source-location-span     #:span])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tests

(module+ test
  (define-check (check-syntax actual-stx expected-datum expected-srcloc)
    (check-pred syntax? actual-stx)
    (check-equal? (syntax->datum actual-stx) expected-datum)
    (check-equal? (syntax-srcloc actual-stx) expected-srcloc))

  (define a-src (srcloc 'a 12 5 144 9))
  (define b-src (srcloc 'b 49 7 343 14))
  (define a-lst (list 'a 12 5 144 9))
  (define b-lst (list 'b 49 7 343 14))
  (define a-vec (vector-immutable 'a 12 5 144 9))
  (define b-vec (vector-immutable 'b 49 7 343 14))
  (define a (datum->syntax #f (list '+ 1 2 3) a-lst))
  (define b (datum->syntax #f (list 'define 'x 987) b-lst))

  (test-case "syntax-srcloc-lens"
    (check-equal? (lens-view syntax-srcloc-lens a) a-src)
    (check-equal? (lens-view syntax-srcloc-lens b) b-src)
    (check-syntax (lens-set syntax-srcloc-lens a a-src) (list '+ 1 2 3) a-src)
    (check-syntax (lens-set syntax-srcloc-lens b b-src) (list 'define 'x 987) b-src)
    (check-syntax (lens-set syntax-srcloc-lens a b-src) (list '+ 1 2 3) b-src)
    (check-syntax (lens-set syntax-srcloc-lens b a-src) (list 'define 'x 987) a-src)
    ;; same thing, but with source-location->srcloc-lens instead of syntax-srcloc-lens
    (check-equal? (lens-view source-location->srcloc-lens a) a-src)
    (check-equal? (lens-view source-location->srcloc-lens b) b-src)
    (check-syntax (lens-set source-location->srcloc-lens a a-src) (list '+ 1 2 3) a-src)
    (check-syntax (lens-set source-location->srcloc-lens b b-src) (list 'define 'x 987) b-src)
    (check-syntax (lens-set source-location->srcloc-lens a b-src) (list '+ 1 2 3) b-src)
    (check-syntax (lens-set source-location->srcloc-lens b a-src) (list 'define 'x 987) a-src)
    ;; same thing, but with source-location->list-lens
    (check-equal? (lens-view source-location->list-lens a) a-lst)
    (check-equal? (lens-view source-location->list-lens b) b-lst)
    (check-syntax (lens-set source-location->list-lens a a-lst) (list '+ 1 2 3) a-src)
    (check-syntax (lens-set source-location->list-lens b b-lst) (list 'define 'x 987) b-src)
    (check-syntax (lens-set source-location->list-lens a b-lst) (list '+ 1 2 3) b-src)
    (check-syntax (lens-set source-location->list-lens b a-lst) (list 'define 'x 987) a-src)
    ;; same thing, but with source-location->vector-lens
    (check-equal? (lens-view source-location->vector-lens a) a-vec)
    (check-equal? (lens-view source-location->vector-lens b) b-vec)
    (check-syntax (lens-set source-location->vector-lens a a-vec) (list '+ 1 2 3) a-src)
    (check-syntax (lens-set source-location->vector-lens b b-vec) (list 'define 'x 987) b-src)
    (check-syntax (lens-set source-location->vector-lens a b-vec) (list '+ 1 2 3) b-src)
    (check-syntax (lens-set source-location->vector-lens b a-vec) (list 'define 'x 987) a-src)
    ;; source-location->srcloc-lens also works with other types of source-locations
    (check-equal? (lens-view source-location->srcloc-lens a-src) a-src)
    (check-equal? (lens-view source-location->srcloc-lens b-src) b-src)
    (check-equal? (lens-view source-location->srcloc-lens a-lst) a-src)
    (check-equal? (lens-view source-location->srcloc-lens b-lst) b-src)
    (check-equal? (lens-view source-location->srcloc-lens a-vec) a-src)
    (check-equal? (lens-view source-location->srcloc-lens b-vec) b-src)
    (check-equal? (lens-set source-location->srcloc-lens a-src b-src) b-src)
    (check-equal? (lens-set source-location->srcloc-lens a-lst b-src) b-lst)
    (check-equal? (lens-set source-location->srcloc-lens a-vec b-src) b-vec)
    (check-equal? (lens-set source-location->srcloc-lens b-src a-src) a-src)
    (check-equal? (lens-set source-location->srcloc-lens b-lst a-src) a-lst)
    (check-equal? (lens-set source-location->srcloc-lens b-vec a-src) a-vec)
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
    ;; same thing, but with source-location-source-lens instead of syntax-source-lens
    (check-equal? (lens-view source-location-source-lens a) 'a)
    (check-equal? (lens-view source-location-source-lens b) 'b)
    (check-syntax (lens-set source-location-source-lens a "bye.rkt")
                  (list '+ 1 2 3)
                  (srcloc "bye.rkt" 12 5 144 9))
    (check-syntax (lens-set source-location-source-lens b "hellooo.rkt")
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
    ;; same thing, but with source-location-line-lens instead of syntax-line-lens
    (check-equal? (lens-view source-location-line-lens a) 12)
    (check-equal? (lens-view source-location-line-lens b) 49)
    (check-syntax (lens-set source-location-line-lens a 8)
                  (list '+ 1 2 3)
                  (srcloc 'a 8 5 144 9))
    (check-syntax (lens-set source-location-line-lens b 11)
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
    ;; same thing, but with source-location-column-lens instead of syntax-column-lens
    (check-equal? (lens-view source-location-column-lens a) 5)
    (check-equal? (lens-view source-location-column-lens b) 7)
    (check-syntax (lens-set source-location-column-lens a 8)
                  (list '+ 1 2 3)
                  (srcloc 'a 12 8 144 9))
    (check-syntax (lens-set source-location-column-lens b 11)
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
    ;; same thing, but with source-location-position-lens instead of syntax-position-lens
    (check-equal? (lens-view source-location-position-lens a) 144)
    (check-equal? (lens-view source-location-position-lens b) 343)
    (check-syntax (lens-set source-location-position-lens a 233)
                  (list '+ 1 2 3)
                  (srcloc 'a 12 5 233 9))
    (check-syntax (lens-set source-location-position-lens b 610)
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
    ;; same thing, but with source-location-span-lens instead of syntax-span-lens
    (check-equal? (lens-view source-location-span-lens a) 9)
    (check-equal? (lens-view source-location-span-lens b) 14)
    (check-syntax (lens-set source-location-span-lens a 10)
                  (list '+ 1 2 3)
                  (srcloc 'a 12 5 144 10))
    (check-syntax (lens-set source-location-span-lens b 15)
                  (list 'define 'x 987)
                  (srcloc 'b 49 7 343 15))
    )
  )
