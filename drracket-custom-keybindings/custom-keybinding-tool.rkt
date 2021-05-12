#lang racket/base

(require drracket/tool
         racket/class
         racket/unit
         racket/runtime-path
         racket/vector
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
      (cond [(pair? kbs/kb-vec)
             (define global-kb (let loop ([kbs kbs/kb-vec])
                                 (cond [(null? kbs)
                                        #f]
                                       [(equal? 'global
                                                (vector-ref (car kbs) 3))
                                        (car kbs)]
                                       [else
                                        (loop (cdr kbs))])))
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
                   [else (error 'drracket-custom-keybindings "Unimplemented 'local kb range")])]
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
