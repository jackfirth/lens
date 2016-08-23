#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [rest-> (-> contract? contract? contract?)]))


(define (rest-> arg-contract result-contract)
  (->* () #:rest (listof arg-contract) result-contract))
