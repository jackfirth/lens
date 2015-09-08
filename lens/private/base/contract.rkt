#lang racket/base

(provide lens/c)

(require racket/contract/base
         "gen-lens.rkt"
         )
(module+ test
  (require rackunit
           racket/contract/region
           fancy-app
           "make-lens.rkt"
           ))

(define (lens/c target/c view/c)
  (rename-contract
   (gen-lens/c
    [lens-view (or/c #f [lens? target/c . -> . view/c])]
    [lens-set (or/c #f [lens? target/c view/c . -> . target/c])]
    [focus-lens (or/c #f [lens? target/c . -> . (values view/c [view/c . -> . target/c])])])
   `(lens/c ,(contract-name target/c) ,(contract-name view/c))))

(module+ test
  (check-exn exn:fail:contract?
             (λ ()
               (define/contract lns (lens/c any/c any/c) #f)
               (void)))
  (define/contract lns (lens/c hash? string?)
    (make-lens (hash-ref _ 'a) (hash-set _ 'a _)))
  (check-equal? (lens-view lns (hash 'a "alpha" 'b "bet"))
                "alpha")
  (check-equal? (lens-set lns (hash 'a "alpha" 'b "bet") "alfa")
                (hash 'a "alfa" 'b "bet"))
  (let-lens [tgt ctxt] lns (hash 'a "alpha" 'b "bet")
    (check-equal? tgt "alpha")
    (check-equal? (ctxt "alfa") (hash 'a "alfa" 'b "bet"))
    (check-exn exn:fail:contract?
               (λ () (ctxt 'alpha))))
  (check-exn exn:fail:contract?
             (λ () (lens-view lns (hash 'a 'alpha 'b 'bet))))
  (check-exn exn:fail:contract?
             (λ () (lens-set lns (hash 'a "alpha" 'b "bet") 'alpha)))
  )
