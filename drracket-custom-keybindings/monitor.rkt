#lang racket

(provide handle)

(require "../kb-base/kb-base/kb-expr.rkt")

(define (handle expanded path the-source orig-cust)
  (unless (exn? expanded)
    (define kb-hash (make-hash))
    (let loop ([s expanded])
      (cond [(syntax? s)
             (when (syntax-property s 'keybinding-info)
               (define prop (syntax-property s 'keybinding-info))
               (cond [(pair? prop)
                      (extract-kb-info-prop kb-hash (car prop))]
                     [else
                      (extract-kb-info-prop kb-hash prop)]))
             (loop (syntax-e s))]
            [(pair? s)
             (loop (car s))
             (loop (cdr s))]))
    kb-hash))

(define (extract-kb-info-prop kbs prop)
  (with-handlers ([exn:fail? (λ (e) (printf "~s\n" e))])
    (define keystroke (vector-ref prop 0))
    (define program-sexp (vector-ref (struct->vector (vector-ref prop 1)) 1))
    (define name (vector-ref prop 2))
    (define active-range (vector-ref prop 3))
    (hash-set! kbs keystroke (vector program-sexp name active-range))))
  