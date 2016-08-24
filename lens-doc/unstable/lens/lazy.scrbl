#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Lazy lenses and recursive lenses}

@defform[(lazy-lens lens-expr)]{
Creates a lazy lens that lazily evaluates @racket[lens-expr] and uses it to view
and set the target.
@lens-unstable-examples[
  (define lazy-first-lens
    (lazy-lens (begin (displayln "evaluating") first-lens)))
  lazy-first-lens
  (lens-view lazy-first-lens '(1 2 3))
  (lens-set lazy-first-lens '(1 2 3) 'a)
]}

@defform[(rec-lens rec-id lens-expr)]{
Creates a potentially recursive lens, where @racket[lens-expr] can refer to
@racket[rec-id] as a lazy version of itself.
@lens-unstable-examples[
  (define (tree-map-lens item-lens)
    (rec-lens the-tree-lens
      (lens-cond [list? (map-lens the-tree-lens)]
                 [else item-lens])))
  (lens-view (tree-map-lens symbol->string-lens) '(a (b (() c)) (d)))
  (lens-set (tree-map-lens symbol->string-lens)
            '(a (b (() c)) (d))
            '("hay" ("bee" (() "sea")) ("deep")))
]}
