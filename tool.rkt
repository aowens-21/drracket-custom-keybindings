#lang racket/base

(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         racket/runtime-path
         mrlib/switchable-button)
(provide tool@)

(define-runtime-path monitor.rkt "monitor.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) (void))
    (define (phase2) (void))
 
    (drracket:module-language-tools:add-online-expansion-handler
     monitor.rkt
     'handle
     (λ (defs-text val)
       (printf "~s\n" (send defs-text get-text 0 (send defs-text last-position)))))))
