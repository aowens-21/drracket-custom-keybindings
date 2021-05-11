#lang racket/base

(require (for-syntax syntax/parse
                     racket/base))

(require (for-syntax "../../tests/kb-base/example-keybindings/plai.rkt"
                     "../../tests/kb-base/example-keybindings/contracts.rkt"
                     "../../tests/kb-base/example-keybindings/misc-racket.rkt"))

(define-syntax (my-cond stx)
  (syntax-property             
   (syntax-parse stx
     #:literals (else)
     [(_ [else expr ...])
      #'(begin expr ...)]
     [(_)
      #'(void)]
     [(_ [q-expr a-expr ...] c-clause ...)
      #'(if q-expr
            (begin a-expr ...)
            (my-cond c-clause ...))])
   'keybinding-info
   (make-kb "c:space"
            swap-cond-branches
            "cond-kb"
            'local
            stx)))

(define-for-syntax (make-kb keystroke kb-base-program name-prefix range stx)
  (vector keystroke
          kb-base-program
          (string-append name-prefix "-" (symbol->string (gensym)))
          range
          (make-srcloc (syntax-source stx)
                       (syntax-line stx)
                       (+ 1 (syntax-column stx))
                       (+ 1 (syntax-position stx))
                       (syntax-span stx))))

(my-cond [(< 1 0) #f]
         [(> 1 0) #t]
         [(= 1 0) "umm..."])
