#lang racket

(module+ test
  (require doc-coverage
           unstable/lens)
  
  (check-all-documented 'unstable/lens))
