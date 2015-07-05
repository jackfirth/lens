#lang racket

(require syntax/parse
         rackunit
         "core/main.rkt"
         (for-syntax racket/syntax
                     syntax/stx
                     syntax/parse))

(provide syntax-lens)


(define-syntax syntax-lens
  (syntax-parser
    [(_ target-name:id template)
     (with-syntax* ([target ((target-stx #'target-name) #'template)]
                    [parse-pattern (template->pattern #'template)]
                    [rebuilder ((template-rebuilder #'target-name) #'parse-pattern)])
       #'(syntax-parser
           [parse-pattern
            (values #'target rebuilder)]))]))

(module+ test
  (define stx-lens (syntax-lens A (_ _ (_ _ A _ _) _ ...)))
  (define stx #'(a b (1 2 3 4 5) c d e f))
  (check-equal? (syntax->datum (lens-view stx-lens stx)) 3)
  (define stx2 (lens-set stx-lens stx #'FOO))
  (check-equal? (syntax->datum stx2) '(a b (1 2 FOO 4 5) c d e f)))


(begin-for-syntax
  
  (define (target-stx target-id)
    (syntax-parser
      [(a ...) (ormap (target-stx target-id) (syntax->list #'(a ...)))]
      [a (and (bound-identifier=? target-id #'a) #'a)]))
  
  (define template->pattern
    (syntax-parser #:literals (_)
      [(a ...) #`(#,@(stx-map template->pattern #'(a ...)))]
      [_ (generate-temporary)]
      [a #'a]))
  
  (define ((template-rebuilder target-id) parse-pattern)
    (with-syntax ([pat parse-pattern])
      #`(lambda (stx)
          (with-syntax ([#,target-id stx])
            #'pat)))))