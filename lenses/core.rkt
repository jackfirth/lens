#lang racket

(require racket/match fancy-app unstable/contract unstable/sequence)

(provide lens/c
         make-lens
         let-lens
         lens-view
         lens-view*
         lens-set
         lens-set*
         lens-transform
         lens-transform*
         lens-compose
         lens-thrush
         identity-lens
         lens-struct
         lens-proc
         )

(module+ test
  (require rackunit))


(define (lens-proc/c input subcomponent)
  (-> input
      (values subcomponent
              (-> subcomponent
                  input))))

(define lens-2-val-context-key
  (make-continuation-mark-key 'lens-2-val-context-key))

(define-syntax-rule (let/immediate-mark key-expr val-id body-expr ...)
  (call-with-immediate-continuation-mark key-expr (lambda (val-id) body-expr ...)))

(struct lens-struct (proc)
  #:property prop:procedure
  (lambda (this target)
    (let/immediate-mark lens-2-val-context-key lens-2-val-context?
      (if lens-2-val-context?
          ((lens-struct-proc this) target)
          (lens-view (lens-struct-proc this) target)))))

(define (lens-proc lns)
  (match lns
    [(lens-struct proc) proc]
    [(? procedure?  proc) proc]))

(define (lens/c target/c view/c)
  (define proc/c (lens-proc/c target/c view/c))
  (if/c lens-struct?
        (struct/c lens-struct proc/c)
        proc/c))

(module+ test
  (define list-lens/c (lens/c list? any/c))
  (check-true (contract? list-lens/c))
  (check-false (flat-contract? list-lens/c)))


(define ((make-lens getter setter) v)
  (values (getter v)
          (setter v _))) ; fancy-app

(define identity-lens
  (values _ identity)) ; fancy-app

(module+ test
  (define (set-first l v)
    (list* v (rest l)))
  (define (set-second l v)
    (list* (first l) v (rest (rest l))))
  (define test-list '(1 2 3))
  (define first-lens (make-lens first set-first))
  (define second-lens (make-lens second set-second))
  (check-equal? (lens-view first-lens test-list) 1)
  (check-equal? (lens-set first-lens test-list 'a) '(a 2 3))
  (check-equal? (lens-view identity-lens 3) 3)
  (check-equal? (lens-set identity-lens 3 4) 4)
  (check-equal? (lens-compose) identity-lens)
  (define first* (lens-struct first-lens))
  (check-equal? (first* test-list) 1)
  (check-equal? (lens-view first* test-list) 1)
  (check-equal? (lens-set first* test-list 'a) '(a 2 3))
  )


(define-syntax-rule (let-lens (view setter) lens-call-expr body ...)
  (let-values ([(view setter) (with-continuation-mark lens-2-val-context-key #t
                                lens-call-expr)])
    body ...))

(module+ test
  (let-lens (view-first setter-first) (first-lens '(1 2 3 4 5))
    (check-eqv? view-first 1)
    (check-equal? (setter-first 'a) '(a 2 3 4 5))))


(define (lens-view lens v)
  (let-lens (view _) (lens v)
    view))

(define (lens-set lens v x)
  (let-lens (_ setter) (lens v)
    (setter x)))

(define (lens-view* v . lenses)
  (for/fold ([v v]) ([lens (in-list lenses)])
    (lens-view lens v)))

(define (lens-set* v . lenses/xs)
  (unless (even? (length lenses/xs))
    (error 'lens-set*
           "expected an even number of association elements\n  association elements: ~v"
           lenses/xs))
  (for/fold ([v v]) ([lens/x (in-slice 2 lenses/xs)])
    (match-define (list lens x) lens/x)
    (lens-set lens v x)))

(module+ test
  (check-eqv? (lens-view first-lens '(1 2 3)) 1)
  (check-equal? (lens-view* '((1 2) 3) first-lens second-lens) 2)
  (check-equal? (lens-set first-lens '(1 2 3) 'a) '(a 2 3))
  (check-equal? (lens-set* '(1 2 3) first-lens 10 second-lens 20) '(10 20 3))
  )


(define (lens-transform lens f v)
  (let-lens (view setter) (lens v)
    (setter (f view))))

(define (lens-transform* v . lenses/fs)
  (unless (even? (length lenses/fs))
    (error 'lens-transform*
           "expected an even number of association elements\n  association elements: ~v"
           lenses/fs))
  (for/fold ([v v]) ([lens/f (in-slice 2 lenses/fs)])
    (match-define (list lens f) lens/f)
    (lens-transform lens f v)))

(module+ test
  (check-equal? (lens-transform first-lens number->string '(1 2 3)) '("1" 2 3))
  (check-equal? (lens-transform* '(1 2 3) first-lens number->string second-lens (* 10 _)) '("1" 20 3))
  )


(define ((lens-compose2 sub-lens super-lens) v)
  (let-lens (super-view super-setter) (super-lens v)
    (let-lens (sub-view sub-setter) (sub-lens super-view)
      (values sub-view
              (compose super-setter sub-setter)))))

(module+ test
  (define first-of-second-lens (lens-compose first-lens second-lens))
  (define first-of-second-lens* (lens-thrush second-lens first-lens))
  (define test-alist '((a 1) (b 2) (c 3)))
  (check-eq? (lens-view first-of-second-lens test-alist) 'b)
  (check-equal? (lens-set first-of-second-lens test-alist 'B)
                '((a 1) (B 2) (c 3)))
  (let-lens [val ctxt] (first-of-second-lens* test-alist)
    (check-equal? val 'b)
    (check-equal? (ctxt 'B) '((a 1) (B 2) (c 3))))
  )


(define ((generalize-operator op) v . vs)
  (if (empty? vs)
      v
      (foldl (λ (next-v previous) (op previous next-v)) v vs)))

(module+ test
  (define (num-append2 n m)
    (+ (* 10 n) m))
  (define num-append (generalize-operator num-append2))
  (check-eqv? (num-append 1 2 3 4 5) 12345)
  (check-eqv? (num-append 1) 1))


(define lens-compose-proc (generalize-operator lens-compose2))

(define lens-compose
  (case-lambda
    [() identity-lens]
    [(v . vs)
     (apply lens-compose-proc v vs)]))

(define (lens-thrush . args)
  (apply lens-compose (reverse args)))

