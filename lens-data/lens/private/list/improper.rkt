#lang racket/base

(provide list-proper-lens
         list-improper-lens)

(require racket/contract
         lens/private/base/contract
         lens/common)

(module+ test
  (require rackunit lens/common lens/private/test-util/test-lens))

;; (-> (Rec R (U (Pairof A R) A)) (Pairof A (Listof A))
(define (make-proper l)
  (if (pair? l)
      (cons (car l)
            (make-proper (cdr l)))
      (list l)))

;; (-> (Pairof A (Listof A)) (Rec R (U (Pairof A R) A)))
(define (make-improper l)
  (if (null? (cdr l))
      (car l)
      (cons (car l)
            (make-improper (cdr l)))))

(define improper-contract
  (flat-rec-contract r
    (cons/c any/c (or/c r (not/c pair?)))))

(define proper-contract
  (flat-rec-contract r
    (cons/c any/c (listof any/c))))

(define/contract list-proper-lens
  (lens/c improper-contract
          proper-contract)
  (make-lens make-proper
             (λ (target view)
               (unless (= (length (make-proper target)) (length view))
                 (raise-argument-error 'stx-flatten/depth-lens
                                       (format "a list of length ~v"
                                               (length (make-proper target)))
                                       1
                                       target
                                       view))
               (make-improper view))))

(define/contract list-improper-lens
  (lens/c proper-contract
          improper-contract)
  (make-lens make-improper
             (λ (target view)
               (unless (= (length (make-proper view)) (length target))
                 (raise-argument-error 'list-improper-lens
                                       (format "an improper list of length ~v (plus the last element)"
                                               (length target))
                                       1
                                       target
                                       view))
               (make-proper view))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (test-case "list-proper-lens and list-improper-lens: lens laws"
    (test-lens-laws list-proper-lens
                    (list* 'x 'y 'z)
                    (list 'a 'b 'c)
                    (list "a" "b" "c"))
    )
  (test-case "list-proper-lens and list-improper-lens: lens-view"
    (check-equal? (lens-view list-proper-lens (list* 'x 'y 'z))
                  (list 'x 'y 'z))
    (check-equal? (lens-view list-proper-lens (list 'x 'y 'z))
                  (list 'x 'y 'z '()))
    (check-equal? (lens-view list-improper-lens (list 'x 'y 'z))
                  (list* 'x 'y 'z))
    (check-equal? (lens-view list-improper-lens (list 'x 'y 'z '()))
                  (list 'x 'y 'z))
    )
  (test-case "list-proper-lens and list-improper-lens: lens-set"
    (check-equal? (lens-set list-proper-lens
                            (list* 'x 'y 'z)
                            (list 1 2 3))
                  (list* 1 2 3))
    (check-equal? (lens-set list-proper-lens
                            (list 'x 'y)
                            (list 1 2 3))
                  (list* 1 2 3))
    (check-equal? (lens-set list-improper-lens
                            (list 'x 'y 'z)
                            (list* 1 2 3))
                  (list 1 2 3))
    (check-equal? (lens-set list-improper-lens
                            (list 'x 'y '())
                            (list* 1 2 3))
                  (list 1 2 3))
    (check-equal? (lens-set list-improper-lens
                            (list 'x 'y 'z)
                            (list 1 2))
                  (list 1 2 '()))
    )
  (test-case "list-proper-lens and list-improper-lens: lens-set length check"
    (check-exn #px"expected: a list of length 3"
               (λ () (lens-set list-proper-lens
                               (list* 'x 'y 'z)
                               (list 1 2))))
    (check-exn #px"expected: a list of length 3"
               (λ () (lens-set list-proper-lens
                               (list* 'x 'y 'z)
                               (list 1 2 3 4))))
    (check-exn #px"expected: a list of length 3"
               (λ () (lens-set list-proper-lens
                               (list 'x 'y)
                               (list 1 2 3 4))))
    (check-exn #px"expected: an improper list of length 3 \\(plus the last element\\)"
               (λ () (lens-set list-improper-lens
                               (list 'x 'y 'z)
                               (list* 1 2))))
    (check-exn #px"expected: an improper list of length 3 \\(plus the last element\\)"
               (λ () (lens-set list-improper-lens
                               (list 'x 'y 'z)
                               (list* 1 2 3 4))))
    (check-exn #px"expected: an improper list of length 3 \\(plus the last element\\)"
               (λ () (lens-set list-improper-lens
                               (list 'x 'y '())
                               (list* 1 2 3 4))))
    ))
