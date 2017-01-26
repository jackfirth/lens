#lang sweet-exp racket/base

provide define-nested-lenses

require lens/private/compound/thrush
        for-syntax racket/base
                   racket/syntax
                   syntax/parse
                   syntax/srcloc
                   "../util/id-append.rkt"

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
    [pattern [suffix-id:id suffix-lens-expr:expr
               unchecked-clause ...]
             #:with base-lens:id base-lens-tmp
             #:do [(define-values [base-suffix-id base-suffix-sub-range]
                     (id-append #:context base-id
                                base-id -- #'suffix-id))
                   (define-values [base-suffix-lens-id base-suffix-lens-sub-range]
                     (id-append #:context base-id
                                base-suffix-id -lens))]
             #:with base-suffix
             base-suffix-id
             #:with base-suffix-lens
             base-suffix-lens-id
             #:with [(~var clause (clause #'base-suffix #'base-suffix-lens)) ...]
             #'[unchecked-clause ...]
             #:with def
             (with-sub-range-binders
              #'(begin
                  (define base-suffix-lens
                    (lens-thrush base-lens suffix-lens-expr))
                  clause.def
                  ...)
              base-suffix-lens-sub-range)])


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

