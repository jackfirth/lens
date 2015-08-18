#lang racket/base

(require racket/contract
         rackunit
         fancy-app
         "../base/base.rkt"
         "../base/view-set.rkt")

(provide
 (contract-out
  [check-view (-> lens? any/c any/c void?)]
  [check-set (-> lens? any/c any/c any/c void?)]
  [check-view-set (-> lens? any/c void?)]
  [check-set-view (-> lens? any/c any/c void?)]
  [check-set-set (-> lens? any/c any/c any/c void?)]
  [test-lens-laws (-> lens? any/c any/c any/c void?)]))


(define-check (check-view lens target expected-view)
  (check-equal? (lens-view lens target) expected-view))

(define-check (check-set lens target new-view expected-new-target)
  (check-equal? (lens-set lens target new-view) expected-new-target))


(define-check (check-view-set lens target)
  (check-equal? (lens-set lens target (lens-view lens target))
                target
                "setting target's view to its own view not equal? to itself"))

(define-check (check-set-view lens target new-view)
  (check-equal? (lens-view lens (lens-set lens target new-view))
                new-view
                "view of target after setting it's view not equal? to the set view"))

(define-check (check-set-set lens target new-view1 new-view2)
  (let* ([target* (lens-set lens target new-view1)]
         [target** (lens-set lens target* new-view2)])
    (check-equal? (lens-view lens target**)
                  new-view2
                  "view of target after setting it's view twice not equal? to second view")))

(define (test-lens-laws lens test-target test-view1 test-view2)
  (check-view-set lens test-target)
  (check-set-view lens test-target test-view1)
  (check-set-view lens test-target test-view2)
  (check-set-set lens test-target test-view1 test-view2))
