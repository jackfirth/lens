#lang racket/base

(module+ test
  (require rackunit
           lens/private/base/main
           lens/private/list/main
           lens/private/vector/main
           lens/private/string/main
           lens/private/compound/if)
  (define if-lens (lens-if list? first-lens (vector-ref-lens 0)))
  (check-equal? (lens-view if-lens '(1 2 3)) 1)
  (check-equal? (lens-view if-lens '#(1 2 3)) 1)
  (check-equal? (lens-set if-lens '(1 2 3) 'a) '(a 2 3))
  (check-equal? (lens-set if-lens '#(1 2 3) 'a) '#(a 2 3))
  (define cond-lens (lens-cond [list? first-lens]
                               [vector? (vector-ref-lens 0)]
                               [string? (string-ref-lens 0)]))
  (check-equal? (lens-view cond-lens '(1 2 3)) 1)
  (check-equal? (lens-view cond-lens '#(1 2 3)) 1)
  (check-equal? (lens-view cond-lens "123") #\1)
  (check-equal? (lens-set cond-lens '(1 2 3) 'a) '(a 2 3))
  (check-equal? (lens-set cond-lens '#(1 2 3) 'a) '#(a 2 3))
  (check-equal? (lens-set cond-lens "123" #\a) "a23")
  (define match-lens (lens-match [(list a) first-lens]
                                 [(list a b) second-lens]
                                 [(list a b c) third-lens]
                                 [(list a ... b) (list-ref-lens (length a))]))
  (check-equal? (lens-view match-lens '(1)) 1)
  (check-equal? (lens-view match-lens '(1 2)) 2)
  (check-equal? (lens-view match-lens '(1 2 3)) 3)
  (check-equal? (lens-view match-lens '(1 2 3 4 5 6)) 6)
  (check-equal? (lens-set match-lens '(1) 'a) '(a))
  (check-equal? (lens-set match-lens '(1 2) 'a) '(1 a))
  (check-equal? (lens-set match-lens '(1 2 3) 'a) '(1 2 a))
  (check-equal? (lens-set match-lens '(1 2 3 4 5 6) 'a) '(1 2 3 4 5 a))
  )
