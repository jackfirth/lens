#lang sweet-exp racket/base

provide stability-notice

require scribble/manual


(define-syntax-rule (stability-notice id)
  (list "This library is stable, backwards compatibility will be maintained. For experimental features see "
                 (racketmodname id) "."))
