#lang racket/base

(require (for-syntax racket/vector
                     racket/base))

(provide make-kb
         attach-keybindings
         keybinding-info-kbs
         keybinding-info
         keybinding-info?)

(struct keybinding-info (kbs) #:prefab)

(define (attach-keybindings stx kbs)
  (syntax-property stx
                   'keybinding-info
                   (cond [(list? kbs)
                          (keybinding-info kbs)]
                         [else (keybinding-info (list kbs))])))

(define (make-kb keystroke kb-base-program name-prefix range stx)
  (vector keystroke
          kb-base-program
          ;; TODO: Fix gensym code here, needs to stay a symbol so just give the name as a base
          (string-append name-prefix "-" (symbol->string (gensym)))
          range
          (make-srcloc (syntax-source stx)
                       (syntax-line stx)
                       (+ 1 (syntax-column stx))
                       (+ 1 (syntax-position stx))
                       (syntax-span stx))))
