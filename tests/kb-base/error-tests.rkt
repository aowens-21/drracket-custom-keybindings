#lang racket/base

(require rackunit
         kb-base
         racket/class
         framework
         "test-utils.rkt")

(check-exn
 exn:fail?
 (lambda ()
   (define e (new racket:text%))
   (run-kb-program (insert 5) e)))

(check-exn
 exn:fail?
 (lambda ()
   (define e (new racket:text%))
   (run-kb-program (do-times (count-iters (set-position 0) 1 'sexp)
                             (insert "a"))
                   e)))

(check-exn
 exn:fail?
 (lambda ()
   (define e (new racket:text%))
   (run-kb-program (seek-while (set-position 0) 1 'simple)
                   e)))

(check-exn
 exn:fail?
 (lambda ()
   (define e (new racket:text%))
   (run-kb-program (count-iters (seq (forward-sexp-exists?)
                                     (set-position 1))
                                1
                                'sexp)
                   e)))

(check-exn
 exn:fail?
 (lambda ()
   (define e (new racket:text%))
   (run-kb-program (kb-let (['b 4])
                     (count-iters (kb-set! 'b (set-position 1))
                                  1
                                  'sexp)
                   e))))