#lang at-exp racket/base

(require racket/require
         (multi-in scribble (base html-properties struct))
         (only-in scribble/core style)
         setup/collects)

(provide other-reference-note see-guide-note see-reference-note)

(define css-resource
  (make-css-addition
   (path->collects-relative
    (collection-file-path "other-reference.css" "lens" "private" "doc-util"))))

(define finger (element (style "margin-note__image-left margin-note__image-left--finger"
                               (list css-resource))
                        '()))

(define (flexible-container . content)
  (element (style "flexible-container" (list css-resource (alt-tag "div"))) content))
(define (flexible-element . content)
  (element (style "flexible-element" (list css-resource (alt-tag "div"))) content))

(define (other-reference-note . pre-content)
  (margin-note (flexible-container finger (apply flexible-element pre-content))))

(define (see-guide-note tag . pre-content)
  @other-reference-note{
    @seclink[tag]{The Lens Guide} has additional examples of @|pre-content|.})

(define (see-reference-note tag . pre-content)
  @other-reference-note{
    @seclink[tag]{The Lens Reference} has additional information on @|pre-content|.})
