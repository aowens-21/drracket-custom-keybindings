#lang racket/base

(provide (struct-out kb-expr)
         (struct-out buffer-safe-kb-expr)
         base-val?
         step-type?
         is-kb-expr?
         is-buffer-safe-kb-expr?)

(struct kb-expr (inner-expr) #:transparent)
(struct buffer-safe-kb-expr kb-expr () #:transparent)

(define (base-val? e)
  (or (number? e)
      (string? e)
      (char? e)
      (boolean? e)))

(define (step-type? t)
  (member t '(simple
              line
              page
              word
              sexp)))

(define (is-kb-expr? e)
  (or (kb-expr? e)
      (base-val? e)
      (step-type? e)
      (symbol? e)))

(define (is-buffer-safe-kb-expr? e)
  (or (buffer-safe-kb-expr? e)
      (base-val? e)
      (step-type? e)
      (symbol? e)))