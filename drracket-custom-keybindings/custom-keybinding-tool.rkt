#lang racket/base

(require drracket/tool
         racket/class
         racket/unit
         racket/runtime-path
         racket/vector
         racket/list
         framework
         "../kb-base/kb-base/interpreter.rkt")

(provide tool@)

(define-runtime-path handler.rkt "handler.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) (void))
    (define (phase2) (void))

    (define kb-keymap (new keymap:aug-keymap%))
    (define (register-to-keymap stroke kbs/kb-vec editor)
      (cond [(list? kbs/kb-vec)
             ;; if there is a global-kb register it, it dominates any local kb's with the same keystroke
             (define global-kb (findf (λ (kb-vec)
                                        (equal? 'global (vector-ref kb-vec 3)))
                                      kbs/kb-vec))
             (cond [global-kb
                    (send kb-keymap
                          add-function
                          (vector-ref global-kb 2)
                          (λ (d e)
                            (send editor begin-edit-sequence)
                            (run-kb-program (vector-ref global-kb 1)
                                            editor)
                            (send editor end-edit-sequence)))
                    (send kb-keymap
                          map-function
                          stroke
                          (vector-ref global-kb 2))]
                   [else
                    (define f-name (vector-ref (first kbs/kb-vec) 2))
                    (send kb-keymap
                          add-function
                          f-name
                          (λ (d e)
                            (define kb-prog #f)
                            (define current-pos (send editor get-end-position))
                            (for ([kb (in-list kbs/kb-vec)]
                                  #:break kb-prog)
                              (define start-kb (car (vector-ref kb 3)))
                              (define end-kb (cdr (vector-ref kb 3)))
                              (when (and (>= current-pos start-kb)
                                         (<= current-pos end-kb))
                                (set! kb-prog (vector-ref kb 1))))
                            (send editor begin-edit-sequence)
                            (run-kb-program kb-prog editor)
                            (send editor end-edit-sequence)))
                    (send kb-keymap
                          map-function
                          stroke
                          f-name)])]
            [(vector? kbs/kb-vec)
             (send kb-keymap
                   add-function
                   (vector-ref kbs/kb-vec 2)
                   (λ (d e)
                     (send editor begin-edit-sequence)
                     (run-kb-program (vector-ref kbs/kb-vec 1)
                                     editor)
                     (send editor end-edit-sequence)))
             (send kb-keymap
                   map-function
                   stroke
                   (vector-ref kbs/kb-vec 2))]
            [else (error 'drracket-custom-keybindings "Keybinding stx property is not well-formed")]))
    
    (drracket:module-language-tools:add-online-expansion-handler
     handler.rkt
     'handle
     (λ (defs-text val)
       (for ([(k v) (in-hash val)])
         (register-to-keymap k v defs-text)
         (send (drracket:rep:get-drs-bindings-keymap) remove-chained-keymap kb-keymap)
         (send (drracket:rep:get-drs-bindings-keymap) chain-to-keymap kb-keymap #f))))))
