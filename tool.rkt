#lang racket/base

(require drracket/tool
         racket/class
         racket/unit
         racket/runtime-path
         racket/vector
         "monitor.rkt")

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
       (printf "~s\n" val)
       (for ([(k v) (in-hash val)])
         (printf "prog: ~s\n" (lookup-kb (vector-ref v 0))))))))
