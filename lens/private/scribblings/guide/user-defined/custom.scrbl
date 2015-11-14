#lang scribble/manual

@(require scribble/eval
          "../../../doc-util/main.rkt")

@(define make-lens-eval
   (make-eval-factory '(racket/base lens)))
@(define-syntax-rule (lens-interaction expr ...)
   (interaction #:eval (make-lens-eval) expr ...))

@(define construct-eval (make-lens-eval))
@(define-syntax-rule (construct-interaction expr ...)
   (interaction #:eval construct-eval expr ...))

@title[#:tag "construction-guide"]{Constructing Entirely New Lenses}

Sometimes the existing set of lenses isn't enough. Perhaps you have a particularly unique data
structure, and you want to create a lens for it. Perhaps you just want to provide lenses for your
custom data structures, and struct lenses are insufficient. In that case, it's always possible to
fall back on the primitive lens constructor, @racket[make-lens].

The @racket[make-lens] constructor is simple---it creates a new lens from a getter function and a
(functional) setter function. That's it. A lens is nothing more than that.

As an example, it would actually be possible to implement lenses for complex numbers: one lens for the
real part and a second lens for the imaginary part. Implementing these lenses is fairly simple---we
just need to write getters and setters for each portion of the number:

@(construct-interaction
  (define real-lens
    (make-lens real-part
               (λ (n r) (make-rectangular (real-part r) (imag-part n)))))
  (define imag-lens
    (make-lens imag-part
               (λ (n i) (make-rectangular (real-part n) (real-part i))))))

In this case, Racket already provides the getters for us: @racket[real-part] and @racket[imag-part].
We need to implement the setters ourselves, which we can do using @racket[make-rectangular]. Now we
can actually do math on separate components of numbers using @racket[lens-transform]:

@(construct-interaction
  (lens-transform real-lens 2+3i (λ (x) (* x 2)))
  (lens-transform imag-lens 2+3i (λ (x) (* x 2))))

When creating a lens with @racket[make-lens], it's important to make sure it also follows the
@seclink["laws"]{lens laws}. These are simple requirements to ensure that your custom lens behaves
intuitively. Lenses that do @emph{not} adhere to these laws will most likely cause unexpected
behavior. However, as long as your lens plays by the rules, it will automatically work with all the
other lens functions, including lens combinators like @racket[lens-compose].
