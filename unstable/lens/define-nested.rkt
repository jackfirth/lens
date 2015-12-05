#lang sweet-exp racket/base

provide define-nested-lenses

require lens/private/compound/thrush
        for-syntax racket/base
                   racket/syntax
                   syntax/parse
                   syntax/srcloc
                   "private/id-append.rkt"
module+ test
  require lens/private/base/base
          lens/private/list/main
          rackunit

begin-for-syntax
  (define (with-sub-range-binders stx prop)
    (syntax-property stx 'sub-range-binders prop))
  (define -- (update-source-location (datum->syntax #f '-)
                                    #:span 1))
  (define -lens (update-source-location (datum->syntax #f '-lens)
                                        #:span 5))
  ;; helper syntax-class for define-nested-lenses
  (define-syntax-class (clause base-id base-lens-tmp)
    #:attributes (def)
    [pattern [suffix-id:id suffix-lens-expr:expr]
             #:with base-lens:id base-lens-tmp
             #:do [(define-values [base-suffix-lens-id sub-range-binders]
                     (id-append #:context base-id
                                base-id -- #'suffix-id -lens))]
             #:with base-suffix-lens
             base-suffix-lens-id
             #:with def
             (with-sub-range-binders
              #`(define base-suffix-lens
                  (lens-thrush base-lens suffix-lens-expr))
              sub-range-binders)])


(define-syntax define-nested-lenses
  (syntax-parser
    [(define-nested-lenses [base:id base-lens-expr:expr]
       (~parse base-lens:id (generate-temporary #'base))
       (~var clause (clause #'base #'base-lens))
       ...)
     #'(begin
         (define base-lens base-lens-expr)
         clause.def
         ...)]))

module+ test
  (define-nested-lenses [first first-lens]
    [first first-lens]
    [second second-lens]
    [third third-lens])
  (check-equal? (lens-view first-first-lens '((a b c d) e)) 'a)
  (check-equal? (lens-view first-second-lens '((a b c d) e)) 'b)
  (check-equal? (lens-view first-third-lens '((a b c d) e)) 'c)

