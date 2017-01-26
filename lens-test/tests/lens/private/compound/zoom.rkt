#lang sweet-exp racket/base

module+ test
  require lens/private/base/main
          lens/private/list/main
          rackunit
          lens/private/compound/zoom
          lens/private/isomorphism/data
          lens/private/list/map
  (define first-sym->str
    (lens-zoom first-lens symbol->string-lens))
  (check-equal? (lens-view first-sym->str '(a b c))
                '("a" b c))
  (check-equal? (lens-set first-sym->str '(a b c) '("a" b c))
                '(a b c))
  (check-equal? (lens-set first-sym->str '(a b c) '("z" b c))
                '(z b c))
  (check-equal? (lens-set first-sym->str '(a b c) '("z" bee sea))
                '(z bee sea))
  (check-equal? (lens-view first-sym->str (lens-set first-sym->str '(a b c) '("z" bee sea)))
                '("z" bee sea))
  (define trans-second-first/third-second
    (lens-zoom* second-lens first-lens third-lens second-lens))
  (check-equal? (lens-view trans-second-first/third-second '(1 (2 3) (4 5)))
                '(1 2 5))
  (check-equal? (lens-set trans-second-first/third-second '(1 (2 3) (4 5)) '(1 2 5))
                '(1 (2 3) (4 5)))
  (check-equal? (lens-set trans-second-first/third-second '(1 (2 3) (4 5)) '(1 b 5))
                '(1 (b 3) (4 5)))
  (check-equal? (lens-set trans-second-first/third-second '(1 (2 3) (4 5)) '(a b c))
                '(a (b 3) (4 c)))
  (check-equal? (lens-view trans-second-first/third-second
                           (lens-set trans-second-first/third-second '(1 (2 3) (4 5)) '(a b c)))
                '(a b c))
  (define (rekey-alist-lens key->new-key-lens)
    (map-lens (lens-zoom car-lens key->new-key-lens)))
  (check-equal? (lens-view (rekey-alist-lens symbol->string-lens) '((a . 1) (b . 2) (c . 3)))
                '(("a" . 1) ("b" . 2) ("c" . 3)))
  (check-equal? (lens-set (rekey-alist-lens symbol->string-lens)
                          '((a . 1) (b . 2) (c . 3))
                          '(("a" . 10) ("b" . 200) ("c" . 3000)))
                '((a . 10) (b . 200) (c . 3000)))
  (check-equal? (lens-set (rekey-alist-lens symbol->string-lens)
                          '((a . 1) (b . 2) (c . 3))
                          '(("one" . 10) ("two" . 200) ("three" . 3000)))
                '((one . 10) (two . 200) (three . 3000)))
  (define (rek+v-alist-lens key->new-key-lens value->new-value-lens)
    (map-lens (lens-zoom* car-lens key->new-key-lens cdr-lens value->new-value-lens)))
  (check-equal? (lens-view (rek+v-alist-lens symbol->string-lens number->string-lens)
                           '((a . 1) (b . 2) (c . 3)))
                '(("a" . "1") ("b" . "2") ("c" . "3")))
  (check-equal? (lens-set (rek+v-alist-lens symbol->string-lens number->string-lens)
                          '((a . 1) (b . 2) (c . 3))
                          '(("a" . "10") ("b" . "200") ("c" . "3000")))
                '((a . 10) (b . 200) (c . 3000)))
  (check-equal? (lens-set (rek+v-alist-lens symbol->string-lens number->string-lens)
                          '((a . 1) (b . 2) (c . 3))
                          '(("one" . "10") ("two" . "200") ("three" . "3000")))
                '((one . 10) (two . 200) (three . 3000)))

