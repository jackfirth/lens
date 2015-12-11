#lang racket/base

(provide struct-id)

(require racket/list
         racket/struct-info
         syntax/parse
         )

(define-syntax-class struct-id
  #:attributes (info constructor-id [accessor-id 1])
  [pattern struct-id:id
           #:attr v (syntax-local-value #'struct-id (λ () #f))
           #:when (struct-info? (attribute v))
           #:attr info (extract-struct-info (attribute v))
           #:with descriptor-id:id (first (attribute info))
           #:with constructor-id:id (syntax-property (second (attribute info))
                                                     'disappeared-use
                                                     (list (syntax-local-introduce #'struct-id)))
           #:with predicate-id:id (third (attribute info))
           #:with [accessor-id:id ...] (reverse (fourth (attribute info)))])

