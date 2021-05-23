#lang racket/base

(require racket/class
         racket/contract
         "interpreter.rkt"
         "kb-expr.rkt"
         framework)

(provide (contract-out [get-kb-program-result (-> (is-a?/c racket:text<%>)
                                                  (or/c base-val? symbol? kb-expr?)
                                                  (values any/c
                                                          exact-nonnegative-integer?
                                                          string?))]
                       [test-kb-program (->* ((or/c base-val? symbol? kb-expr?))
                                             ((-> any/c
                                                  exact-nonnegative-integer?
                                                  string?
                                                  any/c)
                                              #:setup-proc (-> (is-a?/c racket:text<%>)
                                                               any/c))
                                             any/c)]))

(define (get-kb-program-result editor kb-program)
  (define result-val (run-kb-program kb-program editor))
  (values result-val
          (send editor get-end-position)
          (send editor get-text 0 (send editor last-position))))

(define (test-kb-program kb-program
                         [test-proc (λ (val pos text)
                                      (values val pos text))]
                         #:setup-proc [setup-proc
                                       (lambda (txt) (void))])
  (define editor (new racket:text%))
  (setup-proc editor)
  (define-values (result-val result-pos result-text)
    (get-kb-program-result editor kb-program))
  (test-proc result-val result-pos result-text))