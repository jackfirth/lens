#lang racket

(require syntax/parse
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