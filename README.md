lenses [![Build Status](https://travis-ci.org/jackfirth/lenses.svg?branch=master)](https://travis-ci.org/jackfirth/lenses) [![Coverage Status](https://coveralls.io/repos/jackfirth/lenses/badge.svg)](https://coveralls.io/r/jackfirth/lenses) [![Stories in Ready](https://badge.waffle.io/jackfirth/lenses.png?label=ready&title=Ready)](https://waffle.io/jackfirth/lenses) 
===================================
[Documentation](http://pkg-build.racket-lang.org/doc/lenses/index.html)

A Racket package for creating and composing pure functional lenses. A lens is a function that examines a small subpiece of some larger structure. When given a value of that larger structure, the lens returns two values: a *view* value, which is the subpiece, and a *context* function, which accepts a new view value and returns a new structure with the old view replaced by the new view. Think of them as composable, pure functional getters and setters. Examples can be found in the documentation.
