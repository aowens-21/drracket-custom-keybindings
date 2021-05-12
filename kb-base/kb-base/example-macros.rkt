#lang racket/base

(require plai)

(require (for-syntax "../../tests/kb-base/example-keybindings/plai.rkt"
                     "../../tests/kb-base/example-keybindings/contracts.rkt"
                     "../../tests/kb-base/example-keybindings/misc-racket.rkt"
                     syntax/parse
                     racket/base
                     racket/list))

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
   (make-kb "c:b"
            swap-cond-branches
            "cond-kb"
            'global
            stx)))

(define-syntax (my-define-type stx)
   (syntax-parse stx
     [(_ type-name
         [var-name (field-name field-c) field-clause ...]
         ...)
      (define first-var-pos (syntax-position (second (syntax->list stx))))
      (syntax-property
       #'(define-type type-name [var-name (field-name field-c) field-clause ...] ...)
       'keybinding-info
       (make-kb "c:space"
                (gen-type-case (symbol->string (syntax-e #'type-name))
                               first-var-pos)
                "generate-type-case"
                'global
                stx))]))

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
         [(> 1 0) (my-cond [#t #t]
                           [#f #f])]
         [(= 1 0) "umm..."])

(my-define-type Shape
                [circle (r number?)]
                [rect (l number?)
                      (w number?)])
