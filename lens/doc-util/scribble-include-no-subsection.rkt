#lang sweet-exp racket/base

provide scribble-include/no-subsection

require syntax/parse/define
        racket/match
        scribble/core
        for-syntax racket/base
                   syntax/parse


;; scribble-include/no-subsection requires that the module to be included:
;;  - has no title
;;  - has no tag-prefix
;;  - has exactly one (list 'part (generated-tag)) tag
;;  - has no subsections
;;  - has no `to-collect` content
define-syntax scribble-include/no-subsection
  syntax-parser
    (~and stx (scribble-include/no-subsection mod))
     #:with doc-from-mod
     datum->syntax #'mod 'doc
     unless (module-path? (syntax->datum #'mod))
       raise-syntax-error #f
                          "not a module path"
                          #'stx
                          #'mod
     syntax
       begin
         require (only-in mod [doc-from-mod doc])
         match-define
           part #f (list (list 'part (generated-tag))) #f style '() blocks '()
           doc
         nested-flow style blocks
