#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Lenses that transform subpieces}

@defmodule[unstable/lens/transformer]

@defproc[(transformer-lens [lens lens?] [transform-lens lens?]) lens?]{
Creates a lens that transforms the subpiece of the target that @racket[lens]
views with @racket[transform-lens].

@racketblock[(lens-view (transformer-lens lens transform-lens) target)]
is equivalent to:
@racketblock[(lens-transform lens target (λ (v) (lens-view transform-lens v)))]

@lens-unstable-examples[
  (define first-sym->str
    (transformer-lens first-lens symbol->string-lens))
  (lens-view first-sym->str '(a b c))
  (lens-set first-sym->str '(a b c) '("a" b c))
  (lens-set first-sym->str '(a b c) '("z" b c))
  (lens-set first-sym->str '(a b c) '("z" bee sea))
  (lens-view first-sym->str (lens-set first-sym->str '(a b c) '("z" bee sea)))
]}

