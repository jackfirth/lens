#lang racket/base

(require racket/contract/base)
(provide (contract-out
          [string-split-lens
           (-> (or/c immutable-string? char? regexp?)
               (lens/c immutable-string? (listof immutable-string?)))]
          ))

(require racket/match
         racket/string
         lens/base/main
         lens/util/immutable
         )
(module+ test
  (require rackunit))

(define (string-split-lens sep)
  (define sep-rx
    (cond
      [(string? sep) (regexp (regexp-quote sep))]
      [(char? sep) (regexp (regexp-quote (string sep)))]
      [(regexp? sep) sep]
      [else (error 'bad)]))
  (define (get str)
    (map string->immutable-string (regexp-split sep-rx str)))
  (define (set str lst)
    (for ([s (in-list lst)])
      (when (regexp-match? sep-rx s) ; this would violate the lens laws
        (error 'string-split-lens "expected a string not matching ~v, given: ~v" sep s)))
    (define seps (regexp-match* sep-rx str))
    (match-define (cons fst rst) lst)
    (string->immutable-string (string-append* fst (map string-append seps rst))))
  (make-lens get set))

(module+ test
  (define ws-lens (string-split-lens #px"\\s+"))
  (check-equal? (lens-view ws-lens "  foo bar  baz \r\n\t")
                '("" "foo" "bar" "baz" ""))
  (check-equal? (lens-set ws-lens "  foo bar  baz \r\n\t" '("a" "b" "c" "d" "e"))
                "a  b c  d \r\n\te")
  (check-equal? (lens-view ws-lens "a  b c  d \r\n\te")
                '("a" "b" "c" "d" "e"))
  (check-equal? (lens-set ws-lens "a  b c  d \r\n\te" '("" "foo" "bar" "baz" ""))
                "  foo bar  baz \r\n\t")
  (define newline-lens (string-split-lens "\n"))
  (check-equal? (lens-view newline-lens "a,b\nc,d\ne,f,g")
                '("a,b" "c,d" "e,f,g"))
  (check-equal? (lens-set newline-lens "a,b\nc,d\ne,f,g" '("1" "2" "3"))
                "1\n2\n3")
  (define comma-lens (string-split-lens #\,))
  (check-equal? (lens-view comma-lens "a,b,c")
                '("a" "b" "c"))
  (check-equal? (lens-set comma-lens "a,b,c" '("1" "2" "3"))
                "1,2,3")
  )
