#lang scribble/manual

@(require scribble/eval
          "../lenses-examples.rkt"
          "../deflenses.rkt"
          (for-label lenses
                     racket/base
                     racket/contract))


@title{List lenses}

@defproc[(list-ref-lens [n exact-nonnegative-integer?])
         lens?]{
  Returns a lens for viewing the @racket[n]th item of a list,
  with indexing starting from zero.
  @lenses-examples[
    (lens-view (list-ref-lens 3) '(a b c d e f g h))
    (lens-set (list-ref-lens 1) '(a b c d e f g h) 'FOO)
]}

@deflenses[(first-lens
            second-lens
            third-lens
            fourth-lens
            fifth-lens
            sixth-lens
            seventh-lens
            eighth-lens
            ninth-lens
            tenth-lens)]{
  Lenses for examiniming specific items of lists. Shorthands
  for the common use cases of @racket[list-ref-lens].
  @lenses-examples[
    (lens-view third-lens '(a b c d))
    (lens-view (lens-compose second-lens fourth-lens)
               '((a 1) (b 2) (c 3) (d 4)))
]}
