#lang racket

(require fancy-app
         lens
         lens/util/list-pair-contract)

(module+ test
  (require rackunit))

(provide
 (contract-out
  [lens-join/list (->* () #:rest (listof lens?) lens?)]
  [lens-join/hash (->* () #:rest (listof2 any/c lens?) lens?)]
  [lens-join/vector (->* () #:rest (listof lens?) lens?)]
  [lens-join/string (->* () #:rest (listof lens?) lens?)]
  ))


(define (zip xs ys)
  (append-map list xs ys))

(define (lens-join/list . lenses)
  (define (get target)
    (apply lens-view/list target lenses))
  (define (set target new-views)
    (apply lens-set/list target (zip lenses new-views)))
  (make-lens get set))


(module+ test
  (define first-third-fifth-lens
    (lens-join/list first-lens
                    third-lens
                    fifth-lens))
  (check-equal? (lens-view first-third-fifth-lens '(a b c d e f))
                '(a c e))
  (check-equal? (lens-set first-third-fifth-lens '(a b c d e f) '(1 2 3))
                '(1 b 2 d 3 f)))
(define first-first-lens
  (lens-join/list first-lens
                  first-lens))


(define (value-list->hash keys vs)
  (make-immutable-hash (map cons keys vs)))

(define (split-slice n vs)
  (define grouped
    (for/list ([group (in-slice n vs)])
      group))
  (define (get-ith i)
    (map (list-ref _ i) grouped))
  (build-list n get-ith))

(module+ test
  (check-equal? (split-slice 3 '(a 1 FOO b 2 BAR c 3 BAZ))
                '((a b c) (1 2 3) (FOO BAR BAZ))))


(define (lens-join/hash . keys/lenses)
  (match-define (list keys lenses) (split-slice 2 keys/lenses))
  (define list-lens (apply lens-join/list lenses))
  (define (get target)
    (value-list->hash keys (lens-view list-lens target)))
  (define (set target new-view-hash)
    (lens-set list-lens target (map (hash-ref new-view-hash _) keys)))
  (make-lens get set))

(module+ test
  (define a-b-lens (lens-join/hash 'b third-lens
                                   'a first-lens))
  (check-equal? (lens-view a-b-lens '(1 2 3))
                (hash 'a 1 'b 3))
  (check-equal? (lens-set a-b-lens '(1 2 3) (hash 'a 100 'b 200))
                '(100 2 200)))


(define (lens-join/vector . lenses)
  (lens-compose list->vector-lens (apply lens-join/list lenses)))

(define (inverse-function-lens f f-inv)
  (make-lens
   (λ (tgt) (f tgt))
   (λ (tgt v) (f-inv v))))

(define (list->immutable-vector lst)
  (apply vector-immutable lst))

(define list->vector-lens
  (inverse-function-lens list->immutable-vector vector->list))

(module+ test
  (define vector-first-third-fifth-lens
    (lens-join/vector first-lens
                      third-lens
                      fifth-lens))
  (check-equal? (lens-view vector-first-third-fifth-lens '(a b c d e f))
                #(a c e))
  (check-pred immutable? (lens-view vector-first-third-fifth-lens '(a b c d e f)))
  (check-equal? (lens-set vector-first-third-fifth-lens '(a b c d e f) #(1 2 3))
                '(1 b 2 d 3 f)))

(define (lens-join/string . lenses)
  (lens-compose list->string-lens (apply lens-join/list lenses)))

(define (list->immutable-string lst)
  (string->immutable-string (list->string lst)))

(define list->string-lens
  (inverse-function-lens list->immutable-string string->list))

(module+ test
  (define string-first-third-fifth-lens
    (lens-join/string first-lens
                      third-lens
                      fifth-lens))
  (check-equal? (lens-view string-first-third-fifth-lens '(#\a #\b #\c #\d #\e #\f))
                "ace")
  (check-pred immutable? (lens-view string-first-third-fifth-lens '(#\a #\b #\c #\d #\e #\f)))
  (check-equal? (lens-set string-first-third-fifth-lens '(#\a #\b #\c #\d #\e #\f) "ACE")
                '(#\A #\b #\C #\d #\E #\f)))
