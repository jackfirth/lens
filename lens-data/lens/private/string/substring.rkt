#lang racket/base

(require racket/function racket/contract/base unstable/contract)

(provide
 (contract-out
  [substring-lens (->i ([start exact-nonnegative-integer?]
                        [end (start) (and/c exact-nonnegative-integer?
                                            (>=/c start))])
                       [result (start end)
                               (lens/c (string-length->=/c end)
                                       (string-length-=/c (- end start)))])]))

(define (string-length->=/c min)
  (define (length>=? str)
    (>= (string-length str) min))
  (and/c string?
         (rename-contract length>=?
                          `(string-length->=/c ,min))))

(define (string-length-=/c n)
  (define (length=? str)
    (= (string-length str) n))
  (and/c string?
         (rename-contract length=?
                          `(string-length-=/c ,n))))

(require lens/common)

(module+ test
  (require rackunit))

(define (set-substring str start end replacement-str)
  (string-append (substring str 0 start)
                 replacement-str
                 (substring str end)))

(module+ test
  (check-equal? (set-substring "mitten" 0 4 "MITT") "MITTen")
  (check-equal? (set-substring "mitten" 2 4 "ZZ") "miZZen")
  (check-equal? (set-substring "mitten" 2 6 "LLER") "miLLER"))

(define (substring-lens start end)
  (define (substring-lens-getter str)
    (substring str start end))
  (define (substring-lens-setter str replacement-str)
    (set-substring str start end replacement-str))
  (make-lens substring-lens-getter substring-lens-setter))

(module+ test
  (check-pred lens? (substring-lens 2 4))
  (check-equal? (lens-view (substring-lens 2 4) "mitten") "tt")
  (check-equal? (lens-set (substring-lens 2 4) "mitten" "TT") "miTTen"))

(module+ test
  (require (submod ".."))
  (check-exn exn:fail:contract?
             (thunk (substring-lens -1 5)))  ; Improper substring boundaries
  (check-exn exn:fail:contract?
             (thunk (lens-set (substring-lens 2 4) "kitten" "c")))  ; Replacement string is too short
  (check-exn exn:fail:contract?
             (thunk (lens-set (substring-lens 2 4) "kitten" "cat")))  ; Replacement string is too long
  (check-not-exn
             (thunk (lens-set (substring-lens 2 4) "kitten" "ca")))  ; Replacement string is just right!
  )
