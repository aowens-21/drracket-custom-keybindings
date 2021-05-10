#lang racket/base

(require racket/match)

(provide base-val?
         step-type?
         is-kb-expr?
         is-buffer-safe-kb-expr?
         (except-out (struct-out kb-expr)
                     kb-expr?)
         (except-out (struct-out buffer-safe-kb-expr)
                     buffer-safe-kb-expr?))

(struct kb-expr (inner-expr))
(struct buffer-safe-kb-expr kb-expr ())

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