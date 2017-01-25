#lang sweet-exp racket

;; inspired by https://github.com/jackfirth/racket-auto-fix-deps/blob/master/job/src/filter-hash.rkt

provide
  contract-out
    hash-filterer-lens       (-> (-> any/c any/c boolean?) (lens/c immutable-hash? immutable-hash?))
    hash-filterer-lens/key   (-> predicate/c (lens/c immutable-hash? immutable-hash?))
    hash-filterer-lens/value (-> predicate/c (lens/c immutable-hash? immutable-hash?))

require fancy-app
        lens/private/base/main
        lens/private/util/immutable
        racket/hash
module+ test
  require lens/private/test-util/test-lens
          rackunit

(define (hash-filter keep? hsh)
  (for/hash ([(k v) (in-hash hsh)] #:when (keep? k v))
    (values k v)))

(define (hash-filter-not drop? hsh)
  (hash-filter (λ (k v) (not (drop? k v))) hsh))

(define (hash-andmap f hsh)
  (for/and ([(k v) (in-hash hsh)])
    (f k v)))

(define (hash-filterer-lens keep?)
  (make-lens
   (hash-filter keep? _)
   (λ (tgt nvw)
     (unless (hash-andmap keep? nvw)
       (raise-argument-error 'hash-filterer-lens-setter
                             (format "a hash where all key-value pairs pass ~v" keep?)
                             nvw))
     (hash-union (hash-filter-not keep? tgt) nvw))))

(define (hash-filterer-lens/key keep?)
  (hash-filterer-lens (λ (k v) (keep? k))))

(define (hash-filterer-lens/value keep?)
  (hash-filterer-lens (λ (k v) (keep? v))))

module+ test
  (check-lens-view (hash-filterer-lens/key symbol?) (hash 'a 1 "b" 2 'c 3)
                   (hash 'a 1 'c 3))
  (check-lens-set (hash-filterer-lens/key symbol?) (hash 'a 1 "b" 2 'c 3) (hash 'd 4 'e 5)
                  (hash "b" 2 'd 4 'e 5))
  (check-lens-view (hash-filterer-lens/value number?) (hash 'a 1 'b "two" 'c 3)
                   (hash 'a 1 'c 3))
  (check-lens-set (hash-filterer-lens/value number?) (hash 'a 1 'b "two" 'c 3) (hash 'd 4)
                  (hash 'b "two" 'd 4))
  (check-lens-view (hash-filterer-lens =) (hash 1 1.0 2 45 3 3)
                   (hash 1 1.0 3 3))
  (check-lens-set (hash-filterer-lens =) (hash 1 1.0 2 45 3 3) (hash 4 4.0 5.0 5)
                  (hash 2 45 4 4.0 5.0 5))
  (check-exn exn:fail:contract?
             (thunk (lens-set (hash-filterer-lens/key symbol?) (hash 'a 1) (hash "d" 4))))
