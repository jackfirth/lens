#lang scribble/manual

@(require "../doc-util/main.rkt")

@title{Lens Laws}

While @racket[make-lens] allows lenses to be constructed
from arbitrary getters and setters, these getters and setters
should obey some algebraic laws in order for a lens to be a
@italic{proper lens}. A lens that does not obey the lens laws for
all values it can focus on is an @italic{improper lens}.
The lens laws formalize some standard intuitions for how getters
and setters "ought" to work. The laws for lenses are:

@itemlist[
  @item{
    Purity - Viewing and setting with a lens @racket[L] must
    both be pure functions. Formally, the two functions
    @racketblock[
      (λ (target) (lens-view L target))
      (λ (target view) (lens-set L target view))
    ]
    must be pure for tagets and views of @racket[L].
  }
  @item{
    Set-Get Consistency - For all targets and views, setting a target
    and then viewing it returns the set value. Formally, given
    a target @racket[T] of a lens @racket[L], the function
    @racketblock[
      (λ (view)
        (lens-view L (lens-set L T view)))
    ]
    must be identical to the identity function for views of
    @racket[L] with respect to a reasonable definition of equality
    for views of @racket[L].
  }
  @item{
    Get-Set Consistency - For all targets and views, getting a view
    of a target and then setting the target's view to the view you
    just got does nothing. Formally, given a target @racket[T] of
    a lens @racket[L], the expression
    @racketblock[
      (lens-set L T (lens-view L T))
    ]
    must be equal to @racket[T] with respect to a reasonable
    definition of equality for targets of @racket[L].
  }
  @item{
    Last Set Wins - For all targets and views, if you set the same
    target to two different views, the one you set last applies.
    Formally, given a target @racket[T] of a lens @racket[L] and
    two views @racket[v-first] and @racket[v-second] of @racket[L],
    the expression
    @racketblock[
      (lens-view L (lens-set L (lens-set L T v-first) v-second))
    ]
    must be equal to @racket[v-second] with respect to a reasonable
    definition of equality for views of @racket[L].
  }
]

For those familiar with getters and setters in OO languages, none
of these should be surprising other than the requirement that lenses
be pure. The purity of lenses allows them to be composed more
effectively and reasoned about more easily than an impure equivalent
of lenses.

All lenses provided by this library are proper unless otherwise
stated. There is no enforcement or contract that lenses constructed
with functions from this library will always be proper, but individual
functions may provide conditional guarantees about their interactions
with improper lenses and the lens laws
