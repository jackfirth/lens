#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Substring Lenses}

@defproc[(substring-lens [start exact-nonnegative-integer?]
                         [end exact-nonnegative-integer?])
         (lens/c string? string?)]{
  Creates a lens that views a substring from @racket[start] to @racket[end]
  of a given string. @racket[start] is inclusive and @racket[end] is exclusive,
  in the same way as for @racket[substring].
  @lens-unstable-examples[
    (lens-view (substring-lens 1 4) "abcdefg")
    (lens-set (substring-lens 1 4) "abcdefg" "FOO")
  ]
  When setting a new view, the replacement string has to be
  the same length as the span of the substring lens to uphold
  the @seclink["laws"]{lens laws}.
  @lens-unstable-examples[
    (lens-set (substring-lens 1 4) "kitten" "this string is too long!")
]}