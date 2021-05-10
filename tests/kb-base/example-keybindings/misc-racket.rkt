#lang racket/base

(require kb-base)

(provide swap-cond-branches)

(define swap-cond-branches
  (seq (down-sexp)
       (kb-let (['first-sexp-start (get-position)]
                ['first-sexp-end (seq (up-sexp)
                                      (forward-sexp)
                                      (sub (get-position) 1))]
                ['second-sexp-start (seq (down-sexp)
                                         (get-position))]
                ['second-sexp-end (seq (up-sexp)
                                       (forward-sexp)
                                       (sub (get-position) 1))]
                ['first-sexp-content (get-text 'first-sexp-start 'first-sexp-end)]
                ['second-sexp-content (get-text 'second-sexp-start 'second-sexp-end)])
               (set-position 'second-sexp-start)
               (delete 'second-sexp-start 'second-sexp-end)
               (insert 'first-sexp-content)
               (set-position 'first-sexp-start)
               (delete 'first-sexp-start 'first-sexp-end)
               (insert 'second-sexp-content)
               (set-position 'first-sexp-start)
               (up-sexp))))