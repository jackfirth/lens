#lang racket/base
(require lens/private/base/gen-lens
         rackunit
         racket/function)

(struct bad1 ()
  #:methods gen:lens [])
(check-exn #rx"lens-view: not implemented"
           (thunk (lens-view (bad1) 1)))
(check-exn #rx"lens-set: not implemented"
           (thunk (lens-set (bad1) 1 1)))
(check-exn #rx"focus-lens: not implemented"
           (thunk (focus-lens (bad1) 1)))

(struct bad2 ()
  #:methods gen:lens
  [(define (lens-view this tgt) "something")])
(check-equal? (lens-view (bad2) 1) "something")
(check-exn #rx"lens-set: not implemented"
           (thunk (lens-set (bad2) 1 1)))
(check-exn #rx"focus-lens: not implemented"
           (thunk (focus-lens (bad2) 1)))

(struct bad3 ()
  #:methods gen:lens
  [(define (lens-set this tgt nvw) tgt)])
(check-equal? (lens-set (bad3) 1 2) 1)
(check-exn #rx"lens-view: not implemented"
           (thunk (lens-view (bad3) 1)))
(check-exn #rx"focus-lens: not implemented"
           (thunk (focus-lens (bad3) 1)))
