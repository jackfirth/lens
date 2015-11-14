#lang scribble/manual

@(require scribble/eval
          "../../../doc-util/main.rkt")

@(define make-lens-eval
   (make-eval-factory '(racket/base lens)))

@(define-syntax-rule (lens-interaction expr ...)
   (interaction #:eval (make-lens-eval) expr ...))

@title[#:tag "key-value-lenses"]{Lenses on Key-Value Data}

Many Racket data structures hold values that correspond to a given key. Lenses for accessing elements
of these structures by their keys are provided.

@section[#:tag "hash-guide"]{Hash Tables}

@see-reference-note["hash-reference"]{hash lenses}

Racket hash tables are simple key-value associations, and as a result, they only have one primitive
lens constructor, @racket[hash-ref-lens]. Given a key, it produces a lens which views the value
associated with the key:

@(lens-interaction
  (lens-transform (hash-ref-lens 'a) (hash 'a "Hello")
                  (λ (s) (string-append s ", world!"))))

Note that @racket[hash-ref-lens]'s signature differs from that of @racket[hash-ref] in an important
way: it does not accept a “failure result” if the key is missing from the hash. Instead, the lens
always throws an error:

@(lens-interaction
  (lens-view (hash-ref-lens 'not-a-key) (hash)))

This may seem inconvenient, but this limitation is by design---supporting a failure result would
violate one of the @seclink["laws"]{lens laws}. Specifically, “get-set consistency” would no longer
hold. Consider this example:

@(racketblock
  (let ([l (hash-ref-lens 'not-a-key "default")]
        [h (hash)])
    (lens-set l h (lens-view l h))))

If @racket[hash-ref-lens] accepted a default value, then the above expression would produce a new hash
that was not @racket[equal?] to the original target. Enforcing this property makes lenses easier to
reason about, just as ensuring purity makes functions easier to reason about.

Of course, sometimes breaking purity is the easiest way to solve a problem, and similarly, sometimes
breaking the lens laws is okay (though it should be avoided if possible). We could, if we wished,
define our own hash lens that accepts a default value:

@(define ref-default-eval (make-lens-eval))
@(interaction #:eval ref-default-eval
  (define (hash-ref-lens/default key failure-result)
    (make-lens (λ (h)   (hash-ref h key failure-result))
               (λ (h v) (hash-set h key v)))))

With this custom, “naughty” lens, we can actually perform the example from above:

@(interaction #:eval ref-default-eval
  (let ([l (hash-ref-lens/default 'not-a-key "default")]
        [h (hash)])
    (lens-set l h (lens-view l h))))

In addition to @racket[hash-ref-lens], @racket[hash-ref-nested-lens] is provided, which assists in
fetching values from nested hashes. It is defined in terms of @racket[hash-ref-lens] and
@racket[lens-compose], so it is just a shorter way of expressing the same concept:

@(lens-interaction
  (lens-set (hash-ref-nested-lens 'a 'b 'c)
            (hash 'a (hash 'b (hash 'c "foo")))
            "bar"))

@section[#:tag "dict-guide"]{Dictionaries}

@see-reference-note["dict-reference"]{dictionary lenses}

Racket @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{dictionaries} provide a generic
interface for many kinds of key-value data-structures. They encompass hash tables, association lists,
user-defined dictionaries, and even integer-keyed structures like vectors.

In practice, dictionary lenses work identically to lenses on hashes. The @racket[dict-ref-lens]
lens constructor creates a lens with a view that is the value associated with the lens's key.

@(lens-interaction
  (lens-transform (dict-ref-lens 'b)
                  '((a . 1)
                    (b . 2))
                  (λ (x) (* x 2))))
