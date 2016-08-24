#lang racket/base

(provide append*-lens flatten/depth-lens flatten/depth unflatten/depth)

(require fancy-app lens/common racket/list racket/match)

(module+ test
  (require rackunit syntax/parse lens/private/test-util/test-lens))

;; (define-type (Listof* A n)
;;   (cond [(zero? n) A]
;;         [else (Listof* (Listof A) (sub1 n))]))

;; flatten/depth-lens : (Lens (Listof* Any n) (Listof Any))
;; where the only valid views are lists with the same length as the
;; result of (flatten/depth n target)
(define (flatten/depth-lens n)
  (make-lens
   (flatten/depth n _)
   (unflatten/depth n _ _)))

;; append*-lens : (Lens (Listof (Listof Any)) (Listof Any))
;; where the only valid views are lists with the same length as the
;; result of applying append* to the target.
;; Viewing is equivalent to using append*
;; Setting restores the structure of the original nested list
(define append*-lens
  (flatten/depth-lens 2))

;; flatten/depth : n (Listof* A n) -> (Listof A)
(define (flatten/depth n structure)
  (check-structure-depth! n structure)
  (cond [(zero? n) (list structure)]
        [else (append*n (sub1 n) structure)]))

;; unflatten/depth : n (Listof* A n) (Listof B) -> (Listof* B n)
(define (unflatten/depth n structure flattened)
  (check-structure-depth! n structure)
  (check-flattened-length! n structure flattened)
  (cond [(zero? n) (first flattened)]
        [else (unappend*n (sub1 n) structure flattened)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; restore-structure : (Listof (Listof A)) (Listof B) -> (Listof (Listof B))
;; Takes a list of lists and a list and un-flattens the flattened
;; argument according to the structure of the structure arguement.
;; The length of the flattened list must be the same as the length
;; of (append* structure).
(define (restore-structure structure flattened)
  (restore-structure/acc structure flattened (list)))

;; restore-structure/acc : (Listof (Listof A)) (Listof B) (Listof (Listof B)) -> (Listof (Listof B))
;; Accumulates a reversed version of the result of restore-structure,
;; then returns an un-reversed version.
(define (restore-structure/acc structure flattened acc)
  (match structure
    [(list)
     (reverse acc)]
    [(cons s-lst s-rst)
     (define-values [f-lst f-rst]
       (split-at flattened (length s-lst)))
     (restore-structure/acc s-rst f-rst (cons f-lst acc))]))

;; append*n : n (Listof (Listof* A n)) -> (Listof A)
(define (append*n n structure)
  (cond [(zero? n) structure]
        [else (append*n (sub1 n) (append* structure))]))

;; unappend*n : n (Listof (Listof* A n)) (Listof B) -> (Listof (Listof* B n))
(define (unappend*n n structure flattened)
  (cond [(zero? n) flattened]
        [else (restore-structure
               structure
               (unappend*n (sub1 n) (append* structure) flattened))]))

;; list/depth? : Natural Any -> Boolean
(define (list/depth? n structure)
  (cond [(zero? n) #true]
        [else (and (list? structure)
                   (andmap (list/depth? (sub1 n) _) structure))]))

;; check-structure-depth! : n (Listof* A n) -> Void
(define (check-structure-depth! depth structure)
  (unless (list/depth? depth structure)
    (raise-argument-error 'flatten/depth-lens
                          (format "a nested list of depth ~v" depth)
                          structure)))

;; check-flattened-length! : n (Listof* A n) (Listof B) -> Void
(define (check-flattened-length! depth structure flattened)
  (unless (= (length (flatten/depth depth structure)) (length flattened))
    (raise-argument-error 'flatten/depth-lens
                          (format "a list of length ~v"
                                  (length (flatten/depth depth structure)))
                          1
                          structure
                          flattened)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (test-case "append*-lens"
    (check-equal? (lens-view append*-lens (list (list 1) (list 2 3) (list)))
                  (list 1 2 3))
    (check-equal? (lens-set append*-lens
                            (list (list 1) (list 2 3) (list))
                            (list 'a 'b 'c))
                  (list (list 'a) (list 'b 'c) (list)))

    (check-equal? (lens-transform append*-lens
                                  (list (list 1) (list 2 3) (list))
                                  reverse) ; any length-preserving computation
                  (list (list 3) (list 2 1) (list)))

    (check-exn #rx"expected: a nested list of depth 2\n  given: '\\(5\\)"
               (λ () (lens-view append*-lens (list 5))))
    (check-exn #rx"expected: a nested list of depth 2\n  given: '\\(5\\)"
               (λ () (lens-set append*-lens (list 5) (list 'a))))

    (check-exn #rx"expected: a list of length 3\n  given: '\\(a b\\)"
               (λ () (lens-set append*-lens (list (list 1) (list 2 3) (list)) (list 'a 'b))))

    (test-lens-laws append*-lens
                    (list (list 1) (list 2 3) (list))
                    (list 'a 'b 'c)
                    (list "a" "b" "c"))
    )
  
  (test-case "(flatten/depth-lens 0) adds a list layer"
    (define flat0-lens (flatten/depth-lens 0))
    (check-equal? (lens-view flat0-lens 42) (list 42))
    (check-equal? (lens-set flat0-lens 42 (list 'a)) 'a)
    (check-equal? (lens-transform flat0-lens 42 reverse) 42)
    (test-lens-laws flat0-lens
                    42
                    (list 'a)
                    (list "a")))
  (test-case "(flatten/depth-lens 1) copies the list"
    (define flat1-lens (flatten/depth-lens 1))
    (check-equal? (lens-view flat1-lens (list 1 2 3)) (list 1 2 3))
    (check-equal? (lens-set flat1-lens (list 1 2 3) (list 'a 'b 'c)) (list 'a 'b 'c))
    (check-equal? (lens-transform flat1-lens (list 1 2 3) reverse) (list 3 2 1))
    (test-lens-laws flat1-lens
                    (list 1 2 3)
                    (list 'a 'b 'c)
                    (list "a" "b" "c")))
  (test-case "(flatten/depth-lens 2) should be equivalent to append*-lens"
    (define flat2-lens (flatten/depth-lens 2))
    (check-equal? (lens-view flat2-lens
                             (list (list 1) (list 2 3) (list)))
                  (list 1 2 3))
    (check-equal? (lens-set flat2-lens
                            (list (list 1) (list 2 3) (list))
                            (list 'a 'b 'c))
                  (list (list 'a) (list 'b 'c) (list)))
    
    (check-equal? (lens-transform flat2-lens
                                  (list (list 1) (list 2 3) (list))
                                  reverse)
                  (list (list 3) (list 2 1) (list)))
    
    (test-lens-laws flat2-lens
                    (list (list 1) (list 2 3) (list))
                    (list 'a 'b 'c)
                    (list "a" "b" "c")))
  (test-case "(flatten/depth-lens 3) deals with lists of depth 3"
    (define flat3-lens (flatten/depth-lens 3))
    (check-equal? (lens-view flat3-lens
                             (list (list (list) (list 1))
                                   (list (list 2 3))
                                   (list)
                                   (list (list 4) (list) (list 5 6))))
                  (list 1 2 3 4 5 6))
    (check-equal? (lens-set flat3-lens
                            (list (list (list) (list 1))
                                   (list (list 2 3))
                                   (list)
                                   (list (list 4) (list) (list 5 6)))
                            (list 'a 'b 'c 'd 'e 'f))
                  (list (list (list) (list 'a))
                        (list (list 'b 'c))
                        (list)
                        (list (list 'd) (list) (list 'e 'f))))

    (check-equal? (lens-transform flat3-lens
                                  (list (list (list) (list 1))
                                        (list (list 2 3))
                                        (list)
                                        (list (list 4) (list) (list 5 6)))
                                  reverse)
                  (list (list (list) (list 6))
                        (list (list 5 4))
                        (list)
                        (list (list 3) (list) (list 2 1))))

    (check-exn #rx"expected: a nested list of depth 3\n *given: '\\(5\\)"
               (λ () (lens-view flat3-lens (list 5))))
    (check-exn #rx"expected: a nested list of depth 3\n  given: '\\(5\\)"
               (λ () (lens-set flat3-lens (list 5) (list 'a))))

    (check-exn #rx"expected: a list of length 6\n  given: '\\(a b\\)"
               (λ () (lens-set flat3-lens
                               (list (list (list) (list 1))
                                     (list (list 2 3))
                                     (list)
                                     (list (list 4) (list) (list 5 6)))
                               (list 'a 'b))))
    
    (test-lens-laws flat3-lens
                    (list (list (list) (list 1))
                          (list (list 2 3))
                          (list)
                          (list (list 4) (list) (list 5 6)))
                    (list 'a 'b 'c 'd 'e 'f)
                    (list "a" "b" "c" "d" "e" "f")))
  )
