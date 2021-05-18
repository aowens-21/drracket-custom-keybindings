#lang racket/base

(require kb-base)

(provide arrow-to-arrow-star
         arrow-star-to-arrow-i
         func-contract-to-provide-contract)

(define arrow-to-arrow-star
  (seq (delete 2)
       (insert "->* ")
       (kb-let (['num-of-args (sub (count-iters (forward-sexp-exists?) 1 'sexp) 1)])
               (insert "(")
               (delete 1)
               (do-times 'num-of-args (move-position 1 'sexp))
               (insert ")")
               (insert-return)
               (insert "()")
               (insert-return))))

(define arrow-star-to-arrow-i
  (seq (delete 3)
       (insert "->i")
       (kb-let (['dom-list-count (sub (count-iters
                                       (forward-sexp-exists?)
                                       1
                                       'sexp)
                                      1)])
               (do-times 'dom-list-count
                         (down-sexp)
                         (kb-let (['num-of-args (count-iters (forward-sexp-exists?) 1 'sexp)])
                                 (do-times (sub 'num-of-args 1)
                                           (seq (insert "[_ ")
                                                (forward-sexp)
                                                (insert "]")
                                                (insert-return)))
                                 (kb-if (kb-not (kb-equal? 'num-of-args 0))
                                        (seq (insert "[_ ")
                                             (forward-sexp)
                                             (insert "]"))))
                         (up-sexp)
                         (forward-sexp))
               (forward-sexp)
               (backward-sexp)
               (insert "[_ ")
               (forward-sexp)
               (insert "]"))))

;; TODO: The indentation here is horrible, need defines in a seq or something
(define (func-contract-to-provide-contract pos)
  ;; getting the correct text to insert
  (seq (move-position 1 'word)
       (delete 9)
       (down-sexp)
       (kb-let (['func-name (get-forward-word)])
               (up-sexp)
               (forward-sexp)
               (kb-let (['contract-start (get-position)])
                       (forward-sexp)
                       (kb-let (['contract-end (get-position)])
                               (set-position pos)
                               (kb-let (['contract-sexp (get-text (add 'contract-start 3)
                                                                  'contract-end)])
                                       (delete 'contract-start 'contract-end)
    
                                       ;;from the start of the define/contract
                                       (up-sexp)
                                       (move-position -1 'line)
                                       (insert "(provide (contract-out")
                                       (insert-return)
                                       (insert "[")
                                       (insert 'func-name)
                                       (insert " ")
                                       (insert 'contract-sexp)
                                       (insert "]))")
                                       (insert-return)))))))