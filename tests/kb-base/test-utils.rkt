#lang racket/base

(require racket/class
         kb-base
         rackunit
         framework)

(require (for-syntax racket/base
                     syntax/parse
                     rackunit))

(provide get-kb-program-result
         test-kb-program)

;; text<%> kb-program? -> (values integer? string?)
;; give this function an editor and a base kb language program,
;; and it produces the current position in the buffer as well as its content as a string
(define (get-kb-program-result editor kb-program)
  (define result-val (run-kb-program kb-program editor))
  (values result-val
          (send editor get-end-position)
          (send editor get-text 0 (send editor last-position))))

;; kb-program? (any? integer? string? -> void) #:setup-proc (text<%> -> void)
(define (test-kb-program kb-program
                         test-proc
                         #:setup-proc [setup-proc
                                       (lambda (txt) (void))])
  (define editor (new racket:text%))
  (setup-proc editor)
  (define-values (result-val result-pos result-text)
    (get-kb-program-result editor kb-program))
  (test-proc result-val result-pos result-text))