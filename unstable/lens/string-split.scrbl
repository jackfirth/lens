#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Splitting Strings}

@defproc[(string-split-lens [sep (or/c string? char? regexp?)]) lens?]{
Creates a lens that splits a string into multiple pieces like
@racket[regexp-split] or @racket[string-split].
@lens-unstable-examples[
  (lens-view (string-split-lens ",") "a,b,c")
  (lens-set (string-split-lens ",") "a,b,c" '("1" "2" "3"))
]
Lenses created by @racket[string-split-lens] do not trim strings first, so that
when viewing a target that either starts or ends with something matching
@racket[sep], the view will include empty strings as the first or last element,
which is consistant with @racket[regexp-split] or @racket[string-split] with
@racket[#:trim? #f]. This is also more useful when using @racket[lens-set].
@lens-unstable-examples[
  (lens-view (string-split-lens ",") ",b,c")
  (lens-set (string-split-lens ",") ",b,c" '("a" "b" "c"))
  (lens-view (string-split-lens ",") "a,b,c,")
  (lens-set (string-split-lens ",") "a,b,c," '("a" "b" "c" "d"))
]}
