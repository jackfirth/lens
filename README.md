lens [![Build Status](https://travis-ci.org/jackfirth/lens.svg?branch=master)](https://travis-ci.org/jackfirth/lens) [![Coverage Status](https://coveralls.io/repos/jackfirth/lens/badge.svg)](https://coveralls.io/r/jackfirth/lens) [![Stories in Ready](https://badge.waffle.io/jackfirth/lens.png?label=ready&title=Ready)](https://waffle.io/jackfirth/lens) 
===================================
[Documentation](http://pkg-build.racket-lang.org/doc/lens/index.html)

A Racket package for creating and composing pure functional lenses.

`raco pkg install lens`
`(require lens)`

A lens is a value that can be used to focus on a small subpiece of some larger structure. Given a lens and a value of that larger structure, two values can be dervied: a *view* value, which is the subpiece, and a *context* function, which accepts a new view value and returns a new structure with the old view replaced by the new view. Think of them as composable, pure functional getters and setters. Examples can be found in the documentation.

#### Contributions

This project uses Github issues organized by a [Waffle board](https://waffle.io/jackfirth/lens) to track what's being worked on. Check the board to see if there's any features, bugs, etc. that interest you, or create a new Github issue to inquire about something you'd like to see changed.
