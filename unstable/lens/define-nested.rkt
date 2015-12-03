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
  (define-syntax-class (clause base-id)
    [pattern [suffix-id:id suffix-lens-expr:expr]
             #:do [(define-values [base-suffix-lens-id sub-range-binders]
                     (id-append #:context base-id
                                base-id -- #'suffix-id -lens))]
             #:with [base-suffix-lens ...]
             (list base-suffix-lens-id)
             #:with [suffix-lens ...]
             (list #'suffix-lens-expr)
             #:attr sub-range-binders
             sub-range-binders])


(define-syntax define-nested-lenses
  (syntax-parser
    [(define-nested-lenses [base:id base-lens-expr:expr]
       (~var clause (clause #'base))
       ...)
     #:with base-lens:id (generate-temporary #'base)
     #:with [def ...]
     (for/list ([base-suffix-lens-ids (in-list (syntax->list #'[[clause.base-suffix-lens] ... ...]))]
                [suffix-lens-exprs (in-list (syntax->list #'[[clause.suffix-lens ...] ...]))]
                [sub-range-binders-prop (in-list (attribute clause.sub-range-binders))])
       (define/syntax-parse [base-suffix-lens ...] base-suffix-lens-ids)
       (define/syntax-parse [suffix-lens ...] suffix-lens-exprs)
       (with-sub-range-binders
        #`(begin
            (define base-suffix-lens
              (lens-thrush base-lens suffix-lens))
            ...)
        sub-range-binders-prop))
     #'(begin
         (define base-lens base-lens-expr)
         def
         ...)]))

module+ test
  (define-nested-lenses [first first-lens]
    [first first-lens]
    [second second-lens]
    [third third-lens])
  (check-equal? (lens-view first-first-lens '((a b c d) e)) 'a)
  (check-equal? (lens-view first-second-lens '((a b c d) e)) 'b)
  (check-equal? (lens-view first-third-lens '((a b c d) e)) 'c)

