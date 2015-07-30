#lang racket/base

(provide (all-defined-out))

;; The immutable? predicate only works for strings, byte-strings, vectors, hash-tables, and boxes.

(define (immutable-string? v)
  (and (string? v) (immutable? v)))

(define (immutable-bytes? v)
  (and (bytes? v) (immutable? v)))

(define (immutable-vector? v)
  (and (vector? v) (immutable? v)))

(define (immutable-hash? v)
  (and (hash? v) (immutable? v)))

(define (immutable-box? v)
  (and (box? v) (immutable? v)))

(define (list->immutable-string lst)
  (string->immutable-string (list->string lst)))

(define (list->immutable-vector lst)
  (apply vector-immutable lst))

(define (build-immutable-string n f)
  (string->immutable-string (build-string n f)))

(define (build-immutable-vector n f)
  (vector->immutable-vector (build-vector n f)))

