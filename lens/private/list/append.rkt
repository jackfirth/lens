#lang racket/base

(provide append*-lens append*n-lens)

(require "flatten.rkt")

(module+ test
  (require rackunit lens lens/private/test-util/test-lens))

(define (append*n-lens n)
  (flatten/depth-lens (add1 n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (test-case "append*n-lens"
    (define append**-lens (append*n-lens 2))
    (define append***-lens (append*n-lens 3))

    (check-equal? (lens-view append**-lens
                             (list (list (list) (list 1))
                                   (list (list 2 3))
                                   (list)
                                   (list (list 4) (list) (list 5 6))))
                  (list 1 2 3 4 5 6))
    (check-equal? (lens-set append**-lens
                            (list (list (list) (list 1))
                                   (list (list 2 3))
                                   (list)
                                   (list (list 4) (list) (list 5 6)))
                            (list 'a 'b 'c 'd 'e 'f))
                  (list (list (list) (list 'a))
                        (list (list 'b 'c))
                        (list)
                        (list (list 'd) (list) (list 'e 'f))))

    (test-lens-laws append**-lens
                    (list (list (list) (list 1))
                          (list (list 2 3))
                          (list)
                          (list (list 4) (list) (list 5 6)))
                    (list 'a 'b 'c 'd 'e 'f)
                    (list "a" "b" "c" "d" "e" "f"))

    (check-equal? (lens-view append***-lens
                             (list (list (list) (list (list 1)))
                                   (list (list (list) (list 2 3)))
                                   (list)
                                   (list (list (list 4) (list)) (list) (list (list 5 6)))))
                  (list 1 2 3 4 5 6))
    (check-equal? (lens-set append***-lens
                            (list (list (list) (list (list 1)))
                                  (list (list (list) (list 2 3)))
                                  (list)
                                  (list (list (list 4) (list)) (list) (list (list 5 6))))
                            (list 'a 'b 'c 'd 'e 'f))
                  (list (list (list) (list (list 'a)))
                        (list (list (list) (list 'b 'c)))
                        (list)
                        (list (list (list 'd) (list)) (list) (list (list 'e 'f)))))

    (test-lens-laws append**-lens
                    (list (list (list) (list 1))
                          (list (list 2 3))
                          (list)
                          (list (list 4) (list) (list 5 6)))
                    (list 'a 'b 'c 'd 'e 'f)
                    (list "a" "b" "c" "d" "e" "f"))
    (test-lens-laws append***-lens
                    (list (list (list) (list (list 1)))
                          (list (list (list) (list 2 3)))
                          (list)
                          (list (list (list 4) (list)) (list) (list (list 5 6))))
                    (list 'a 'b 'c 'd 'e 'f)
                    (list "a" "b" "c" "d" "e" "f"))
    ))
