#lang sweet-exp racket/base

provide lens-join/struct

require racket/local
        racket/match
        lens/private/base/main
        kw-make-struct
        for-syntax racket/base
                   syntax/parse
module+ test
  require rackunit lens/private/hash/main lens/private/test-util/test-multi

(begin-for-syntax
  (define-splicing-syntax-class field-lenses
    #:attributes ([lens-expr 1] [lens-id 1] [vw-id 1] [norm 1])
    [pattern (~seq lens-expr:expr ...)
      #:with [lens-id ...] (generate-temporaries #'[lens-expr ...])
      #:with [vw-id ...] (generate-temporaries #'[lens-expr ...])
      #:with [norm ...] #'[vw-id ...]]
    [pattern (~seq fst-lens:expr ...+ rst:field-lenses)
      #:with [fst-lens-id ...] (generate-temporaries #'[fst-lens ...])
      #:with [fst-vw-id ...] (generate-temporaries #'[fst-lens ...])
      #:with [lens-expr ...] #'[fst-lens ... rst.lens-expr ...]
      #:with [lens-id ...] #'[fst-lens-id ... rst.lens-id ...]
      #:with [vw-id ...] #'[fst-vw-id ... rst.vw-id ...]
      #:with [norm ...] #'[fst-vw-id ... rst.norm ...]]
    [pattern (~seq (~seq kw:keyword fst-lens:expr) ...+ rst:field-lenses)
      #:with [fst-lens-id ...] (generate-temporaries #'[fst-lens ...])
      #:with [fst-vw-id ...] (generate-temporaries #'[fst-lens ...])
      #:with [lens-expr ...] #'[fst-lens ... rst.lens-expr ...]
      #:with [lens-id ...] #'[fst-lens-id ... rst.lens-id ...]
      #:with [vw-id ...] #'[fst-vw-id ... rst.vw-id ...]
      #:with [[fst-kw/vw-id ...] ...] #'[[kw fst-vw-id] ...]
      #:with [norm ...] #'[fst-kw/vw-id ... ... rst.norm ...]]
    ))

(define-syntax lens-join/struct
  (lambda (stx)
    (syntax-parse stx
      [(lens-join/struct s:id flds:field-lenses)
       #:with make/kw-form (syntax/loc stx (make/kw s flds.norm ...))
       #:with [[lens-id/vw-id ...] ...] #'[[flds.lens-id flds.vw-id] ...]
       #`(local [(define flds.lens-id flds.lens-expr) ...]
           (make-lens
            (λ (tgt)
              (define flds.vw-id (lens-view flds.lens-id tgt))
              ...
              make/kw-form)
            (λ (tgt nvw)
              (match-define make/kw-form nvw)
              (lens-set/list tgt lens-id/vw-id ... ...))))])))

(module+ test
  (struct foo (a b c) #:transparent)
  (define foo-hash-lens1
    (lens-join/struct foo
                      (hash-ref-lens 'a)
                      (hash-ref-lens 'b)
                      (hash-ref-lens 'c)))
  (define foo-hash-lens2
    (lens-join/struct foo
                      #:a (hash-ref-lens 'a)
                      #:b (hash-ref-lens 'b)
                      #:c (hash-ref-lens 'c)))
  (define foo-hash-lens3
    (lens-join/struct foo
                      #:c (hash-ref-lens 'c)
                      #:a (hash-ref-lens 'a)
                      #:b (hash-ref-lens 'b)))
  (define foo-hash-lens4
    (lens-join/struct foo
                      (hash-ref-lens 'a)
                      #:c (hash-ref-lens 'c)
                      #:b (hash-ref-lens 'b)))
  (test-multi* ([foo-hash-lens #:in [foo-hash-lens1 foo-hash-lens2 foo-hash-lens3 foo-hash-lens4]])
    (check-equal? (lens-view foo-hash-lens (hash 'a 1 'b 2 'c 3))
                  (foo 1 2 3))
    (check-equal? (lens-set foo-hash-lens (hash 'a 1 'b 2 'c 3) (foo 10 20 30))
                  (hash 'a 10 'b 20 'c 30))
    ))
