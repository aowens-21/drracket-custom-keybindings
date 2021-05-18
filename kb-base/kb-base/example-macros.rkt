#lang racket/base

(require plai
         (prefix-in contract: racket/contract))

(require (for-syntax "../../tests/kb-base/example-keybindings/plai.rkt"
                     "../../tests/kb-base/example-keybindings/contracts.rkt"
                     "../../tests/kb-base/example-keybindings/misc-racket.rkt"
                     "../../kb-base/kb-base/helpers.rkt"
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
            'local
            stx)))

(my-cond [(= 1 0) "umm..."]
         [(> 1 0) (my-cond [#f #f]
                           [#t #t])]
         [(< 1 0) #f])

(define-syntax (my-define-type stx)
   (syntax-parse stx
     [(_ type-name
         [var-name (field-name field-c) field-clause ...]
         ...)
      (define first-var-pos (- (syntax-position (third (syntax->list stx))) 1))
      (syntax-property
       #'(define-type type-name [var-name (field-name field-c) field-clause ...] ...)
       'keybinding-info
       (make-kb "c:space"
                (gen-type-case (symbol->string (syntax-e #'type-name))
                               first-var-pos)
                "generate-type-case"
                'global
                stx))]))

(my-define-type Shape
                [circle (r number?)]
                [rect (l number?)
                      (w number?)])

(define-syntax (-> stx)
  (syntax-parse stx
    [(_ mandatory-dom ... range)
     (syntax-property
      #'(contract:-> mandatory-dom ... range)
      'keybinding-info
      (make-kb "c:a"
               arrow-to-arrow-star
               "arrow-to-arrow-star"
               'local
               stx))]))

(define/contract (my-f n)
  (-> number? string?)
  (make-string n #\a))

(define-syntax (->* stx)
  (syntax-parse stx
    [(_ (mandatory-dom ...) (optional-dom ...) range)
     (syntax-property
      #'(contract:->* (mandatory-dom ...) (optional-dom ...) range)
      'keybinding-info
      (make-kb "c:q"
               arrow-star-to-arrow-i
               "arrow-star-to-arrow-i"
               'local
               stx))]))

(define/contract (other-f n [c #\a])
  (->* (number?)
       (char?)
       string?)
  (make-string n c))