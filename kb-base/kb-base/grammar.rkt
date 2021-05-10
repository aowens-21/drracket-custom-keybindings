#lang racket/base

(require redex)

;; NOTE:
;; ------------------
;; This grammar has a big problem in that it doesn't allow characters as a datatype and
;; thus cannot be used as a faithful grammar for the keybinding base language. I'm keeping
;; it because it might be useful in the future but allowing characters as a base value type
;; is necessary.

(define-language kb-base
  (expr ::=
        (insert expr)
        (delete expr expr)
        (insert-return)
        (seq expr expr)
        (add expr expr)
        (sub expr expr)
        (kb-let id expr expr)
        (kb-set! id expr)
        (set-position expr)
        (forward-sexp)
        (backward-sexp)
        (up-sexp)
        (down-sexp)
        (move-position expr step-type)
        (kb-if expr expr expr)
        (do-times expr expr)
        (seek-while buffer-safe-expr expr step-type)
        (get-forward-sexp)
        (get-position)
        (last-position)
        (count-iters buffer-safe-expr expr step-type)
        (get-text expr expr)
        (get-forward-word expr)
        (get-character)
        (kb-not expr)
        (kb-equal? expr expr)
        (forward-sexp-exists?)
        base-val
        id)

  (buffer-safe-expr ::=
                    (seq buffer-safe-expr buffer-safe-expr)
                    (add buffer-safe-expr buffer-safe-expr)
                    (sub buffer-safe-expr buffer-safe-expr)
                    (kb-let id buffer-safe-expr buffer-safe-expr)
                    (kb-set! id buffer-safe-expr)
                    (kb-if buffer-safe-expr buffer-safe-expr buffer-safe-expr)
                    (do-times buffer-safe-expr buffer-safe-expr)
                    (get-forward-sexp)
                    (get-position)
                    (last-position)
                    (count-iters buffer-safe-expr buffer-safe-expr step-type)
                    (get-text buffer-safe-expr buffer-safe-expr)
                    (get-forward-word buffer-safe-expr)
                    (get-character)
                    (kb-not buffer-safe-expr)
                    (kb-equal? buffer-safe-expr buffer-safe-expr)
                    (forward-sexp-exists?)
                    base-val
                    id)

  (base-val ::=
            number
            string
            boolean)

  (step-type ::=
             'simple
             'word
             'page
             'line
             'sexp)

  (id ::= variable-not-otherwise-mentioned))      