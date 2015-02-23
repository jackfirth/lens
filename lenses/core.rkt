#lang racket

(provide lens/c
         let-lens
         lens-view
         lens-set
         lens-transform
         lens-compose)

(define (lens/c input subcomponent)
  (-> input
      (values subcomponent
              (-> subcomponent
                  input))))

(define-syntax-rule (let-lens (view setter) lens-call-expr body ...)
  (let-values ([(view setter) lens-call-expr])
    body ...))

(define (lens-view lens v)
  (let-lens (view _) (lens v)
    view))

(define (lens-set lens v x)
  (let-lens (_ setter) (lens v)
    (setter x)))

(define (lens-transform lens v f)
  (let-lens (view setter) (lens v)
    (setter (f view))))

(define ((lens-compose2 sub-lens super-lens) v)
  (let-lens (super-view super-setter) (super-lens v)
    (let-lens (sub-view sub-setter) (sub-lens super-view)
      (values sub-view
              (compose super-setter sub-setter)))))

(define ((generalize-operator op) v . vs)
  (if (empty? vs)
      v
      (foldl (λ (next-v previous) (op previous next-v)) vs)))

(define lens-compose (generalize-operator lens-compose2))