#lang sweet-exp racket/base

require lens/private/base/base
        lens/private/base/transform
        racket/list
        prefix-in v: lens/private/list/main
        prefix-in f: "list-ref-focus.rkt"

(define (test n list-ref-lens)
  (define lst (range n))
  (for ([i (in-range n)])
    (lens-transform (list-ref-lens i) lst add1)))

(for ([n (in-list '(1 10 100 1000 10000 100000))])
  (time (test n v:list-ref-lens))
  (time (test n f:list-ref-lens)))

;cpu time: 1 real time: 1 gc time: 0
;cpu time: 0 real time: 1 gc time: 0
;cpu time: 1 real time: 2 gc time: 0
;cpu time: 0 real time: 0 gc time: 0
;cpu time: 18 real time: 17 gc time: 0
;cpu time: 0 real time: 1 gc time: 0
;cpu time: 311 real time: 402 gc time: 71
;cpu time: 29 real time: 28 gc time: 11
;cpu time: 5000 real time: 5075 gc time: 785
;cpu time: 1948 real time: 1979 gc time: 668
;cpu time: 504018 real time: 510507 gc time: 191361
;cpu time: 277455 real time: 281503 gc time: 153934
