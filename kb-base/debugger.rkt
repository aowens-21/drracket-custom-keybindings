#lang racket/base

(require kb-base
         racket/gui/base
         framework
         racket/class)

(provide open-debugger)

(define (open-debugger)
  (define debugger-frame
    (new frame%
         [label "Editor Language Debugger"]
         [width 1000]
         [height 600]))

  (define top-level-panel
    (new vertical-panel%
         [parent debugger-frame]))

  (define debugger-panel
    (new horizontal-panel%
         [parent top-level-panel]
         [horiz-margin 10]))

  (define sandbox-buffer
    (new racket:text%))
  (define sandbox-editor
    (new editor-canvas%
         [parent debugger-panel]
         [editor sandbox-buffer]))

  (define working-buffer
    (new racket:text%))
  (define working-editor
    (new editor-canvas%
         [parent debugger-panel]
         [editor working-buffer]))

  (define run-program-btn
    (new button%
         [label "Run"]
         [parent top-level-panel]
         [callback
          (λ (btn evt)
            (define working-content-as-port
              (open-input-text-editor working-buffer))

            (define read-working-content (read working-content-as-port))
            (run-kb-program
             (eval read-working-content)
             sandbox-buffer))]))
  (send debugger-frame show #t))
       
