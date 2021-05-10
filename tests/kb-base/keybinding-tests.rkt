#lang racket/base

(require racket/class
         rackunit
         kb-base
         "test-utils.rkt"
         "example-keybindings/contracts.rkt"
         "example-keybindings/misc-racket.rkt"
         "example-keybindings/plai.rkt")

(test-kb-program
 swap-cond-branches
 (lambda (result-val result-pos result-text)
   (check-equal? result-text "(cond [(> 2 3) #f]\n      [(< 1 4) #t]\n      [else #f])")
   (check-equal? result-pos 6))
 #:setup-proc (lambda (editor)
                (send editor insert "(cond [(< 1 4) #t]\n      [(> 2 3) #f]\n      [else #f])")
                (send editor set-position 6)))

(test-kb-program
 (gen-type-case "Shape" 21)
 (lambda (result-val result-pos result-text)
   (check-equal? result-text "(define-type Shape\n  [circle (r number?)]\n  [rect (l number?)\n        (w number?)])\n\n(type-case Shape ...\n  [circle (r) ...]\n  [rect (l w) ...])\n  ")
   (check-equal? result-pos 144))
 #:setup-proc (lambda (editor)
                (send editor insert "(define-type Shape\n  [circle (r number?)]\n  [rect (l number?)\n        (w number?)])")))

(test-kb-program
 (arrow-to-arrow-star 45)
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 85)
   (check-equal? result-text "(define/contract (my-contracted-func x y)\n  (->* (integer? string?)\n       ()\n       boolean?)\n  (> x (string-length y)))"))
 #:setup-proc (lambda (editor)
                (send editor insert "(define/contract (my-contracted-func x y)\n  (-> integer? string? boolean?)\n  (> x (string-length y)))")))

(test-kb-program
 (arrow-star-to-arrow-i 45)
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 113)
   (check-equal? result-text "(define/contract (my-contracted-func x y)\n  (->i ([_ integer?]\n        [_ string?])\n       ()\n       [_ boolean?])\n  (> x (string-length y)))"))
 #:setup-proc (lambda (editor)
                (send editor insert "(define/contract (my-contracted-func x y)\n  (->* (integer? string?)\n       ()\n       boolean?)\n  (> x (string-length y)))")))

(test-kb-program
 (func-contract-to-provide-contract 1)
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 87)
   (check-equal? result-text "(provide (contract-out\n          [my-contracted-func (-> integer? string? boolean?)]))\n(define (my-contracted-func x y)\n (> x (string-length y)))"))
 #:setup-proc (lambda (editor)
                (send editor insert "(define/contract (my-contracted-func x y)\n  (-> integer? string? boolean?)\n (> x (string-length y)))")
                (send editor set-position 1)))
