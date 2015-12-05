#lang racket/base

(require racket/contract/base unstable/contract)

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

(require lens)

(module+ test
  (require rackunit))

(define (set-substring str start end replacement-str)
  (string-append (substring str 0 start)
                 replacement-str
                 (substring str end)))

(module+ test
  (check-equal? (set-substring "kitten" 0 4 "KITT") "KITTen")
  (check-equal? (set-substring "kitten" 2 6 "ller") "killer")
  (check-equal? (set-substring "kitten" 2 4 "ss") "kissen"))

(define (substring-lens start end)
  (define (substring-lens-getter str)
    (substring str start end))
  (define (substring-lens-setter str replacement-str)
    (set-substring str start end replacement-str))
  (make-lens substring-lens-getter substring-lens-setter))

(module+ test
  (check-pred lens? (substring-lens 2 4))
  (check-equal? (lens-view (substring-lens 2 4) "kitten") "tt")
  (check-equal? (lens-set (substring-lens 2 4) "kitten" "TT") "kiTTen"))
