#lang scribble/manual

@(require lens/doc-util/main)

@title{lens-view/thrush, lens-set/thrush, and lens-transform/thrush}

@defmodule[unstable/lens/arrow]

@deftogether[[@defproc[(lens-view/thrush [target any/c] [lens lens?] ...) any/c]
              @defproc[(lens-view~> [target any/c] [lens lens?] ...) any/c]]]{
Like @racket[lens-view], except that it can take multiple lenses,
which are combined into a nested lens. The argument order is
switched, so that the @racket[target] comes first and the		
@racket[lens] arguments come after it.		
@racket[(lens-view/thrush target lens ...)] produces the same value as		
@racket[(lens-view (lens-thrush lens ...) target)], but can be more		
efficient.
The function @racket[lens-view~>] is provided as a shorter version.
@lenses-unstable-examples[
  (lens-view/thrush '(a b ((c d) e f) g) third-lens first-lens second-lens)
  (lens-view~> '(a b ((c d) e f) g) third-lens first-lens second-lens)
]}

@deftogether[[@defproc[(lens-set/thrush [target any/c] [lens lens?] ... [#:-> new-view any/c]) any/c]
              @defproc[(lens-set~> [target any/c] [lens lens?] ... [#:-> new-view any/c]) any/c]]]{
Like @racket[lens-set], except that it can take multiple lenses,
which again are combined into a nested lens. 	
@racket[(lens-set/thrush target lens ... #:-> new-view)] is equivalent
to @racket[(lens-set (lens-thrush lens ...) target new-view)], and
@racket[lens-set~>] is the shorter version.
@lenses-unstable-examples[
  (lens-set/thrush '(a b ((c d) e f) g) third-lens first-lens second-lens #:-> "sea")
  (lens-set~> '(a b ((c d) e f) g) third-lens first-lens second-lens #:-> "sea")
]}

@deftogether[[@defproc[(lens-transform/thrush [target any/c] [lens lens?] ...
                                              [#:-> transformer (-> any/c any/c)])
                       any/c]
              @defproc[(lens-transform~> [target any/c] [lens lens?] ...
                                         [#:-> transformer (-> any/c any/c)])
                       any/c]]]{
Like @racket[lens-transform], except that it can take multiple lenses,
just like @racket[lens-set/thrush].	
@racket[(lens-transform/thrush target lens ... #:-> transformer)] is
equivalent to
@racket[(lens-transform (lens-thrush lens ...) target transformer)],
and @racket[lens-transform~>] is the shorter verison.
@lenses-unstable-examples[
  (lens-transform/thrush '(a b ((c d) e f) g) third-lens first-lens second-lens #:-> symbol->string)
  (lens-transform~> '(a b ((c d) e f) g) third-lens first-lens second-lens #:-> symbol->string)
]}

