#lang info

(define version "0.1")
(define pkg-authors '(aowens-21))
(define pkg-desc "A plugin and language for implementing macro-specific keybinding extensions for DrRacket")
(define collection 'multi)
(define deps '("rackunit-lib"
               "base"
               "drracket-plugin-lib"
               "gui-lib"
               "plai-lib"
               "redex-gui-lib"))
(define build-deps '("gui-doc"
                     "racket-doc"
                     "rackunit-lib"
                     "scribble-lib"))
(define test-omit-paths '("drracket-custom-keybindings/custom-keybinding-tool.rkt"))
