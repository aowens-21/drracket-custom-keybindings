#lang racket/base

(require drracket/tool
         racket/class
         racket/unit
         racket/runtime-path
         racket/vector
         framework
         "../kb-base/kb-base/interpreter.rkt")

(provide tool@)

(define-runtime-path monitor.rkt "monitor.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) (void))
    (define (phase2) (void))

    (define kb-keymap (new keymap:aug-keymap%))
    (define (register-to-keymap stroke vec editor)
      (send kb-keymap
            add-function
            (vector-ref vec 1)
            (λ (d e)
              (run-kb-program (vector-ref vec 0)
                              editor)))
      (send kb-keymap
            map-function
            stroke
            (vector-ref vec 1)))
    
    (drracket:module-language-tools:add-online-expansion-handler
     monitor.rkt
     'handle
     (λ (defs-text val)
       (for ([(k v) (in-hash val)])
         (register-to-keymap k v defs-text)
         (send (drracket:rep:get-drs-bindings-keymap) remove-chained-keymap kb-keymap)
         (send (drracket:rep:get-drs-bindings-keymap) chain-to-keymap kb-keymap #f))))))
