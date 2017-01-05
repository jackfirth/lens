#lang racket/base

(provide stx->list-lens
         stx-map-lens
         stx-car-lens
         stx-cdr-lens
         stx-caar-lens
         stx-cdar-lens
         stx-cadr-lens
         stx-cddr-lens
         stx-caaar-lens
         stx-cdaar-lens
         stx-cadar-lens
         stx-cddar-lens
         stx-caadr-lens
         stx-cdadr-lens
         stx-caddr-lens
         stx-cdddr-lens
         stx-append*-lens
         stx-flatten/depth-lens
         stx-append*n-lens
         )

(require fancy-app lens/common lens/private/list/main racket/list racket/match syntax/stx)

(module+ test
  (require rackunit syntax/parse lens/private/test-util/test-lens))

;; stx-e : Any -> Any
(define (stx-e stx)
  (if (syntax? stx)
      (syntax-e stx)
      stx))

;; restore-stx : (case-> [Stx Any -> Stx]
;;                       [Any Any -> Any])
(define (restore-stx stx dat)
  (if (syntax? stx)
      ;; Preserve the distinction between #'(a . (b c)) and #'(a b c)
      (let loop ([stx1 stx]
                 [dat1 dat])
        (cond
          [(syntax? stx1)
           (datum->syntax stx1 (loop (syntax-e stx1) dat1) stx1 stx1)]
          [(and (pair? stx1) (pair? dat1))
           (cons (car dat1)
                 (loop (cdr stx1) (cdr dat1)))]
          [(null? stx1)
           stx1]
          [else
           dat1]))
      dat))

(define stx-e-lens
  (make-lens
   stx-e
   restore-stx)) ; the target will be used as the context

;; stx->list* : (Stx-Listof Any) -> (Listof Any)
(define (stx->list* stx)
  (define lst (stx->list stx))
  ;; lst : (U (Listof Any) False)
  (unless lst (error 'stx->list* "expected a stx-list, given ~v" stx))
  ;; lst : (Listof Any)
  lst)

(define stx->list-lens
  (make-lens
   stx->list*
   restore-stx))

(define (stx-map-lens elt-lens)
  (make-lens
   (lens-view/stx-map elt-lens _)
   (lens-set/stx-map elt-lens _ _)))

(define (lens-view/stx-map lens tgts)
  (stx-map (lens-view lens _) tgts))

(define (lens-set/stx-map lens tgts new-views)
  (restore-stx tgts
    (stx-map (lens-set lens _ _) tgts new-views)))

(define stx-car-lens (lens-thrush stx-e-lens car-lens))
(define stx-cdr-lens (lens-thrush stx-e-lens cdr-lens))
(define stx-caar-lens (lens-thrush stx-car-lens stx-car-lens))
(define stx-cdar-lens (lens-thrush stx-car-lens stx-cdr-lens))
(define stx-cadr-lens (lens-thrush stx-cdr-lens stx-car-lens))
(define stx-cddr-lens (lens-thrush stx-cdr-lens stx-cdr-lens))
(define stx-caaar-lens (lens-thrush stx-caar-lens stx-car-lens))
(define stx-cdaar-lens (lens-thrush stx-caar-lens stx-cdr-lens))
(define stx-cadar-lens (lens-thrush stx-cdar-lens stx-car-lens))
(define stx-cddar-lens (lens-thrush stx-cdar-lens stx-cdr-lens))
(define stx-caadr-lens (lens-thrush stx-cadr-lens stx-car-lens))
(define stx-cdadr-lens (lens-thrush stx-cadr-lens stx-cdr-lens))
(define stx-caddr-lens (lens-thrush stx-cddr-lens stx-car-lens))
(define stx-cdddr-lens (lens-thrush stx-cddr-lens stx-cdr-lens))

;; stx-append* : (Stx-Listof (Stx-Listof A)) -> (Stx-Listof A)
(define (stx-append* lol)
  (append* (stx-map stx->list* lol)))

;; restore-structure : (Stx-Listof (Stx-Listof A)) (Stx-Listof B) -> (Stx-Listof (Stx-Listof B))
;; Takes a list of lists and a list and un-flattens the flattened
;; argument according to the structure of the structure arguement.
;; The length of the flattened list must be the same as the length
;; of (stx-append* structure).
(define (restore-structure structure flattened)
  (match (stx-e structure)
    [(list)
     (unless (stx-null? flattened)
       (error 'stx-append*-lens "flattened list is too long to match the structure"))
     structure]
    [(cons s-lst s-rst)
     (define-values [f-lst f-rst]
       (stx-split-at flattened (stx-length s-lst)))
     (restore-stx structure
       (cons (restore-stx s-lst f-lst)
             (restore-structure s-rst f-rst)))]))


;; stx-flatten/depth-lens : (Lens (Stx-Listof* Any n) (Stx-Listof Any))
;; where the only valid views are stx-lists with the same length as
;; the result of (stx-flatten/depth n target)
(define (stx-flatten/depth-lens n)
  (make-lens
   (stx-flatten/depth n _)
   (stx-unflatten/depth n _ _)))

;; stx-append*-lens : (Lens (Stx-Listof (Stx-Listof Any)) (Stx-Listof Any))
;; where the only valid views are stx-lists with the same length as
;; the result of applying stx-append* to the target.
;; Viewing is equivalent to using stx-append*
;; Setting restores the structure of the original nested stx-list
(define stx-append*-lens
  (stx-flatten/depth-lens 2))

;; stx-flatten/depth : n (Stx-Listof* A n) -> (Stx-Listof A)
(define (stx-flatten/depth n lst*)
  (check-structure-depth! n lst*)
  (cond [(zero? n) (list lst*)]
        [else (stx-append*n (sub1 n) lst*)]))

;; stx-unflatten/depth : n (Stx-Listof* A n) (Stx-Listof B) -> (Stx-Listof* B n)
(define (stx-unflatten/depth n lst* lst)
  (check-structure-depth! n lst*)
  (check-flattened-length! n lst* lst)
  (cond [(zero? n)
         (match-define (list v) (stx->list* lst))
         v]
        [else
         (stx-unappend*n (sub1 n) lst* lst)]))

;; stx-append*n : n (Stx-Listof (Stx-Listof* A n)) -> (Stx-Listof A)
(define (stx-append*n n lst*)
  (cond [(zero? n) lst*]
        [else (stx-append*n (sub1 n) (stx-append* lst*))]))

;; stx-unappend*n : n (Stx-Listof (Stx-Listof* A n)) (Stx-Listof B) -> (Stx-Listof (Stx-Listof* B n))
(define (stx-unappend*n n lst* lst)
  (cond [(zero? n) lst]
        [else (restore-structure
               lst*
               (stx-unappend*n (sub1 n) (stx-append* lst*) lst))]))

(define (stx-append*n-lens n)
  (stx-flatten/depth-lens (add1 n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stx-list/depth? : Natural Any -> Boolean
(define (stx-list/depth? n structure)
  (cond [(zero? n) #true]
        [else (and (stx-list? structure)
                   (stx-andmap (stx-list/depth? (sub1 n) _) structure))]))

;; check-structure-depth! : n (Stx-Listof* A n) -> Void
(define (check-structure-depth! depth structure)
  (unless (stx-list/depth? depth structure)
    (raise-argument-error 'stx-flatten/depth-lens
                          (format "a nested stx-list of depth ~v" depth)
                          structure)))

;; check-flattened-length! : n (Stx-Listof* A n) (Stx-Listof B) -> Void
(define (check-flattened-length! depth structure flattened)
  (unless (= (stx-length (stx-flatten/depth depth structure)) (stx-length flattened))
    (raise-argument-error 'stx-flatten/depth-lens
                          (format "a stx-list of length ~v"
                                  (stx-length (stx-flatten/depth depth structure)))
                          1
                          structure
                          flattened)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stx-length : (Stx-Listof A) -> Natural
(define (stx-length lst)
  (length (stx->list* lst)))

;; stx-andmap : [A -> Boolean] (Stx-Listof A) -> Boolean
(define (stx-andmap f lst)
  (andmap f (stx->list* lst)))

;; stx-split-at : (Stx-Listof A) Natural -> (values (Listof A) (Stx-Listof A))
(define (stx-split-at lst* pos*)
  (let loop ([acc (list)] [pos pos*] [lst lst*])
    (cond [(zero? pos)
           (values (reverse acc) lst)]
          [(stx-null? lst)
           (error 'stx-split-at "index is too large for stx-list\n  index: ~v\n  stx-list: ~v"
                  pos* lst*)]
          [else
           (loop (cons (stx-car lst) acc)
                 (sub1 pos)
                 (stx-cdr lst))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define a* #'a)
  (define b* #'b)
  (define c* #'c)
  (define 1* #'1)
  (define 2* #'2)
  (define 3* #'3)
  (test-case "restore-stx"
    (let ([one-dot-two-three (restore-stx #'(a . (b c)) '(1 2 3))])
      ;; Check that the result has the shape #'(1 . (2 3)) and not #'(1 2 3)
      (check-true (syntax? one-dot-two-three))
      (check-true (pair? (syntax-e one-dot-two-three)))
      (check-true (syntax? (cdr (syntax-e one-dot-two-three))))))
  (test-case "stx-e-lens"
    (check-equal? (lens-view stx-e-lens a*) 'a)
    (check-equal? (syntax-e (lens-set stx-e-lens a* 1)) 1)
    (check-equal? (lens-view stx-e-lens 'a) 'a)
    (check-equal? (lens-set stx-e-lens 'a 1) 1)
    (check-equal? (lens-view stx-e-lens #`(#,a* #,b* #,c*)) (list a* b* c*))
    (check-equal? (syntax-e (lens-set stx-e-lens #`(#,a* #,b* #,c*) (list 1* 2* 3*)))
                  (list 1* 2* 3*))
    (check-equal? (lens-view stx-e-lens (list a* b* c*)) (list a* b* c*))
    (check-equal? (lens-set stx-e-lens (list a* b* c*) (list 1* 2* 3*)) (list 1* 2* 3*))
    )
  (test-case "stx->list-lens"
    (check-equal? (lens-view stx->list-lens #`(#,a* #,b* #,c*))
                  (list a* b* c*))
    (check-equal? (syntax->list (lens-set stx->list-lens #`(#,a* #,b* #,c*) (list 1* 2* 3*)))
                  (list 1* 2* 3*))
    (check-exn #rx"expected a stx-list, given #<syntax.* 5>"
               (λ () (lens-view stx->list-lens #'5)))
    (test-case "stx->list-lens preserves the distinction between #'(a . (b c)) and #'(a b c)"
      (let ([one-dot-two-three (lens-set stx->list-lens #'(a . (b c)) '(1 2 3))])
        ;; Check that the result has the shape #'(1 . (2 3)) and not #'(1 2 3)
        (check-true (syntax? one-dot-two-three))
        (check-true (pair? (syntax-e one-dot-two-three)))
        (check-true (syntax? (cdr (syntax-e one-dot-two-three)))))
      )
    )
  
  (test-case "(stx-map-lens stx->list-lens)"
    (check-equal? (lens-view (stx-map-lens stx->list-lens) #`((#,a*) (#,b* #,c*) ()))
                  (list (list a*) (list b* c*) (list)))
    (check-equal? (stx-map syntax->list
                           (lens-set (stx-map-lens stx->list-lens)
                                     #`((#,a*) (#,b* #,c*) ())
                                     (list (list 1*) (list 2* 3*) (list))))
                  (list (list 1*) (list 2* 3*) (list)))
    )
  (test-case "stx-car-lens, stx-cdr-lens, etc."
    (check-equal? (lens-view stx-car-lens #`(#,a* . #,b*)) a*)
    (check-equal? (lens-view stx-cdr-lens #`(#,a* . #,b*)) b*)
    (check-equal? (lens-view stx-car-lens (cons a* b*)) a*)
    (check-equal? (lens-view stx-cdr-lens (cons a* b*)) b*)
    (check-equal? (syntax-e (lens-set stx-car-lens #`(#,a* . #,b*) 1*)) (cons 1* b*))
    (check-equal? (syntax-e (lens-set stx-cdr-lens #`(#,a* . #,b*) 1*)) (cons a* 1*))
    (check-equal? (lens-set stx-car-lens (cons a* b*) 1*) (cons 1* b*))
    (check-equal? (lens-set stx-cdr-lens (cons a* b*) 1*) (cons a* 1*))
    (check-equal? (lens-view stx-car-lens #`(#,a* #,b* #,c*)) a*)
    (check-equal? (lens-view stx-cadr-lens #`(#,a* #,b* #,c*)) b*)
    (check-equal? (lens-view stx-caddr-lens #`(#,a* #,b* #,c*)) c*)
    (check-equal? (lens-view stx-car-lens (list a* b* c*)) a*)
    (check-equal? (lens-view stx-cadr-lens (list a* b* c*)) b*)
    (check-equal? (lens-view stx-caddr-lens (list a* b* c*)) c*)
    (check-equal? (syntax-e (lens-set stx-car-lens #`(#,a* #,b* #,c*) 1*)) (list 1* b* c*))
    (check-equal? (syntax-e (lens-set stx-cadr-lens #`(#,a* #,b* #,c*) 1*)) (list a* 1* c*))
    (check-equal? (syntax-e (lens-set stx-caddr-lens #`(#,a* #,b* #,c*) 1*)) (list a* b* 1*))
    (check-equal? (lens-set stx-car-lens (list a* b* c*) 1*) (list 1* b* c*))
    (check-equal? (lens-set stx-cadr-lens (list a* b* c*) 1*) (list a* 1* c*))
    (check-equal? (lens-set stx-caddr-lens (list a* b* c*) 1*) (list a* b* 1*))
    )
  (test-case "stx-append*-lens"
    (check-equal? (lens-view stx-append*-lens (list (list 1*) (list 2* 3*) (list)))
                  (list 1* 2* 3*))
    (check-equal? (lens-view stx-append*-lens #`((#,1*) (#,2* #,3*) ()))
                  (list 1* 2* 3*))
    (check-equal? (lens-set stx-append*-lens
                            (list (list 1) (list 2 3) (list))
                            (list 'a 'b 'c))
                  (list (list 'a) (list 'b 'c) (list)))
    (check-equal? (map syntax->list
                       (lens-set stx-append*-lens
                                 (list #`(#,1*) #`(#,2* #,3*) #`())
                                 (list a* b* c*)))
                  (list (list a*) (list b* c*) (list)))
    (check-equal? (map syntax->list
                       (syntax-e
                        (lens-set stx-append*-lens
                                  #`((#,1*) (#,2* #,3*) ())
                                  (list a* b* c*))))
                  (list (list a*) (list b* c*) (list)))

    (check-equal? (lens-transform stx-append*-lens
                                  (list (list 1) (list 2 3) (list))
                                  (lambda (lst)
                                    ;; a length-preserving computation
                                    (let loop ([acc (list)] [sum 0] [lst lst])
                                      (match lst
                                        [(list) (reverse acc)]
                                        [(cons fst rst)
                                         (loop (cons (+ sum fst) acc)
                                               (+ sum fst)
                                               rst)]))))
                  (list (list 1) (list 3 6) (list)))

    (check-equal? (map syntax->datum
                       (syntax-e
                        (lens-transform
                         stx-append*-lens
                         #'(((+ a)) ((- a b) (* c d)) ())
                         (lambda (lst)
                           ;; a length-preserving computation
                           (syntax-parse
                               (expand #`(#%expression (λ (a b c d) (#%app list #,@lst))))
                             #:literals (#%plain-lambda #%plain-app list)
                             [(#%expression (#%plain-lambda (x ...) (#%plain-app list e ...)))
                              #'[e ...]])))))
                  (list (list '(#%app + a))
                        (list '(#%app - a b) '(#%app * c d))
                        (list)))

    (check-exn #rx"expected: a nested stx-list of depth 2\n  given: '\\(5\\)"
               (λ () (lens-view stx-append*-lens (list 5))))
    (check-exn #rx"expected: a nested stx-list of depth 2\n  given: '\\(5\\)"
               (λ () (lens-set stx-append*-lens (list 5) (list 'a))))

    (check-exn #rx"expected: a stx-list of length 3\n  given: '\\(a b\\)"
               (λ () (lens-set stx-append*-lens (list (list 1) (list 2 3) (list)) (list 'a 'b))))

    (test-lens-laws stx-append*-lens
                    (list (list 1) (list 2 3) (list))
                    (list 'a 'b 'c)
                    (list "a" "b" "c"))
    (test-lens-laws stx-append*-lens
                    (list (list 1*) (list 2* 3*) (list))
                    (list a* b* c*)
                    (list "a" "b" "c"))
    (test-case "stx-append*-lens preserves the distinction between #'([a . (b c)] …) and #'([a b c] …)"
      (let ([with-dots (lens-set stx-append*-lens
                                 #'([a . (b c)] [d e . (f)] . ([g h]))
                                 '(1 2 3 4 5 6 7 8))])
        ;; Check that the result has the shape #'([1 . (2 3)] [4 5 . (6)] . ([7 8]))
        ;; and not #'([1 2 3] [4 5 6] [7 8])
        (define (show-shape e)
          (cond
            [(syntax? e) `(stx ,(show-shape (syntax-e e)))]
            [(pair? e) (cons (show-shape (car e))
                             (show-shape (cdr e)))]
            [else e]))
        (check-equal? (show-shape with-dots)
                      (show-shape #'([1 . (2 3)] [4 5 . (6)] . ([7 8]))))
        (check-not-equal? (show-shape with-dots)
                          (show-shape #'([1 2 3] [4 5 6] [7 8]))))
      )
    )
  (test-case "stx-flatten/depth-lens"
    (define flat0-lens (stx-flatten/depth-lens 0))
    (define flat1-lens (stx-flatten/depth-lens 1))
    (define flat2-lens (stx-flatten/depth-lens 2))
    (define flat3-lens (stx-flatten/depth-lens 3))
    (define flat4-lens (stx-flatten/depth-lens 4))

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

    (test-lens-laws flat3-lens
                    (list (list (list) (list 1))
                          (list (list 2 3))
                          (list)
                          (list (list 4) (list) (list 5 6)))
                    (list 'a 'b 'c 'd 'e 'f)
                    (list "a" "b" "c" "d" "e" "f"))

    (check-equal? (lens-view flat4-lens
                             (list (list (list) (list (list 1)))
                                   (list (list (list) (list 2 3)))
                                   (list)
                                   (list (list (list 4) (list)) (list) (list (list 5 6)))))
                  (list 1 2 3 4 5 6))
    (check-equal? (lens-set flat4-lens
                            (list (list (list) (list (list 1)))
                                  (list (list (list) (list 2 3)))
                                  (list)
                                  (list (list (list 4) (list)) (list) (list (list 5 6))))
                            (list 'a 'b 'c 'd 'e 'f))
                  (list (list (list) (list (list 'a)))
                        (list (list (list) (list 'b 'c)))
                        (list)
                        (list (list (list 'd) (list)) (list) (list (list 'e 'f)))))

     (check-exn #rx"expected: a nested stx-list of depth 3\n *given: '\\(5\\)"
               (λ () (lens-view flat3-lens (list 5))))
    (check-exn #rx"expected: a nested stx-list of depth 3\n  given: '\\(5\\)"
               (λ () (lens-set flat3-lens (list 5) (list 'a))))

    (check-exn #rx"expected: a stx-list of length 6\n  given: '\\(a b\\)"
               (λ () (lens-set flat3-lens
                               (list (list (list) (list 1))
                                     (list (list 2 3))
                                     (list)
                                     (list (list 4) (list) (list 5 6)))
                               (list 'a 'b))))
    
   (test-lens-laws flat0-lens
                    42
                    (list 'a)
                    (list "a"))
    (test-lens-laws flat1-lens
                    (list 1 2 3)
                    (list 'a 'b 'c)
                    (list "a" "b" "c"))
    (test-lens-laws flat2-lens
                    (list (list 1) (list 2 3) (list))
                    (list 'a 'b 'c)
                    (list "a" "b" "c"))
    (test-lens-laws flat3-lens
                    (list (list (list) (list 1))
                          (list (list 2 3))
                          (list)
                          (list (list 4) (list) (list 5 6)))
                    (list 'a 'b 'c 'd 'e 'f)
                    (list "a" "b" "c" "d" "e" "f"))
    (test-lens-laws flat4-lens
                    (list (list (list) (list (list 1)))
                          (list (list (list) (list 2 3)))
                          (list)
                          (list (list (list 4) (list)) (list) (list (list 5 6))))
                    (list 'a 'b 'c 'd 'e 'f)
                    (list "a" "b" "c" "d" "e" "f"))
    ))
