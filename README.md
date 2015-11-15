# lens ![Version](https://img.shields.io/badge/Version-2.0-green.svg) [![Build Status](https://travis-ci.org/jackfirth/lens.svg?branch=master)](https://travis-ci.org/jackfirth/lens) [![Coverage Status](https://coveralls.io/repos/jackfirth/lens/badge.svg)](https://coveralls.io/r/jackfirth/lens) [![Stories in Ready](https://badge.waffle.io/jackfirth/lens.png?label=ready&title=Ready)](https://waffle.io/jackfirth/lens) [![Scribble Docs](https://img.shields.io/badge/Docs-Scribble%20-blue.svg)](http://pkg-build.racket-lang.org/doc/lens/index.html)

A Racket package for creating and composing pure functional lenses.

`raco pkg install lens`
`(require lens)`

### What on earth are lenses?

A lens is a value that can be used to focus on a small subpiece of some larger structure. A lens splits some data structure into two pieces - a *view*, which is some small isolated component of the data structure, and a *context*, which is everything else. The context can have a new view placed into it. This makes a lens act like a pure functional getter and setter:

```racket
> (lens-view first-lens '(1 2 3))
1
> (lens-set first-lens '(1 2 3) 'a)
'(a 2 3)
```

Lenses are first class values and pure functional, so they can be abstracted over and functions that operate on lenses can be created. For instance, given a lens its view can be "updated":

```racket
> (lens-transform first-lens '(1 2 3) number->string)
'("1" 2 3)
```

Additionaly, lenses are separate values from the objects they operate on, so they can be manipulated independently of any specific data. Functions can construct lenses, and operations can combine lenses. This allows for *lens composition*:

```racket
> (define first-of-b-key-lens (lens-compose first-lens (hash-ref-lens 'b)))
> (define a-hash (hash 'a '(1 2 3) 'b '(10 20 30) 'c '(100 200 300)))
> (lens-view first-of-b-key-lens a-hash)
10
> (lens-set first-of-b-key-lens a-hash 'foo)
#hash((a . (1 2 3)) (b . (foo 20 30)) (c . (100 200 300)))
```

Lenses can also be joined together to form compound lenses that view many things:

```racket
> (define first-third-fifth-lens (lens-join/list first-lens third-lens fifth-lens))
> (lens-view first-third-fifth-lens '(1 2 3 4 5 6))
'(1 3 5)
> (lens-set first-third-fifth-lens '(1 2 3 4 5 6) '(a b c))
'(a 2 b 4 c 6)
```

Lenses can also be extended to operate on some new data structure:

```racket
> (define first-of-each-lens (map-lens first-lens))
> (lens-view first-of-each-lens '((1 2) (3 4) (5 6)))
'(1 3 5)
> (lens-set first-of-each-lens '((1 2) (3 4) (5 6)) '(a b c))
'((a 2) (b 4) (c 6))
```

See [the documentation](http://pkg-build.racket-lang.org/doc/lens/index.html) for a full API reference

#### So when would I want to use lenses?

Lenses are most effective when you're dealing with the "giant ball of state" problem. When you
have a large amount of state you need to pass around between code written in a functional
style, it's difficult to update and manage it due to the lack of mutation "magically" updating
your entire object graph when a function changes a small part of it. Lenses allow code to
break down and manipulate portions of this state, simplifying interactions and updates.

In particular, consider using lenses if you find yourself doing any of the following:

- Using a giant complex piece of state that most pieces of code only care about a small part of
- Writing `struct-copy` a lot
- Converting some hairy data structure into another one, manipulating it, then turning it back
- Wishing you could treat data X as if it were a Y, i.e. "I wish this struct was a list so I could `map` over it easily"
- Creating structs that have nested struct instances inside them.

For a more in depth introduction, see [The Lens Guide](http://pkg-build.racket-lang.org/doc/lens/lens-guide.html). For detailed API documentation, see [The Lens Reference](http://pkg-build.racket-lang.org/doc/lens/lens-reference.html).

#### Contributions

This project uses Github issues organized by a [Waffle board](https://waffle.io/jackfirth/lens) to track what's being worked on. Check the board to see if there's any features, bugs, etc. that interest you, or create a new Github issue to inquire about something you'd like to see changed.
