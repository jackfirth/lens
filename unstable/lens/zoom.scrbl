#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Lenses that transform subpieces}

@defmodule[unstable/lens/zoom]

@defproc[(lens-zoom [zoom-lens lens?] [transform-lens lens?]) lens?]{
Creates a lens that transforms the subpiece of the target that @racket[zoom-lens]
views with @racket[transform-lens].

@racketblock[(lens-view (lens-zoom zoom-lens transform-lens) target)]
is equivalent to:
@racketblock[(lens-transform zoom-lens target (λ (v) (lens-view transform-lens v)))]

@lens-unstable-examples[
  (define first-sym->str
    (lens-zoom first-lens symbol->string-lens))
  (lens-view first-sym->str '(a b c))
  (lens-set first-sym->str '(a b c) '("a" b c))
  (lens-set first-sym->str '(a b c) '("z" b c))
  (lens-set first-sym->str '(a b c) '("z" bee sea))
  (lens-view first-sym->str (lens-set first-sym->str '(a b c) '("z" bee sea)))
]}

@defproc[(lens-zoom* [zoom-lens lens?] [transform-lens lens?] ... ...) lens?]{
A multi-arg version of @racket[lens-zoom], analogous to
@racket[lens-transform/list]. It is equivalent to
@racket[(lens-thrush (lens-zoom zoom-lens transform-lens) ...)].
@lens-unstable-examples[
  (define first-sym->str/second-num->str
    (lens-zoom* first-lens symbol->string-lens second-lens number->string-lens))
  (lens-view first-sym->str/second-num->str '(a 2 c))
  (lens-set first-sym->str/second-num->str '(a 2 c) '("a" "2" c))
  (lens-set first-sym->str/second-num->str '(a 2 c) '("z" "3" c))
  (lens-set first-sym->str/second-num->str '(a 2 c) '("z" "3" sea))
  (lens-view first-sym->str/second-num->str
             (lens-set first-sym->str/second-num->str '(a 2 c) '("z" "3" sea)))
]}

