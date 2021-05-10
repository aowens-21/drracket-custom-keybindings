#lang info

(define collection "kb-base")
(define pkg-desc "The core of a language for writing keybinding/editor operation behavior.")
(define verison "1.0")
(define pkg-authors '(aowens-21))
(define deps '("base"
               "gui-lib"
               "rackunit-lib"))
(define scribblings '(("scribblings/kb-base.scrbl" (multi-page))))
(define build-deps '("scribble-lib"))
