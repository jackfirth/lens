#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Converting between structs and lists}

@deftogether[[
  @defform[(struct->list-lens struct-id)]
  @defform[(list->struct-lens struct-id)]]]{
Lenses that convert between structs and lists.

@lens-unstable-examples[
  (struct foo (a b c) #:transparent)
  (lens-view (struct->list-lens foo) (foo 1 2 3))
  (lens-set (struct->list-lens foo) (foo 1 2 3) '(4 5 6))
  (lens-view (list->struct-lens foo) '(1 2 3))
  (lens-set (list->struct-lens foo) '(1 2 3) (foo 4 5 6))
  (struct bar foo (d e))
  (lens-view (struct->list-lens bar) (bar 1 2 3 4 5))
  (lens-set (struct->list-lens bar) (bar 1 2 3 4 5) '(6 7 8 9 10))
  (lens-view (list->struct-lens bar) '(1 2 3 4 5))
  (lens-set (list->struct-lens bar) '(1 2 3 4 4) (bar 6 7 8 9 10))
]}
