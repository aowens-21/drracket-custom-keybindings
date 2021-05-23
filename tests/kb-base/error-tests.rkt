#lang racket/base

(require rackunit
         kb-base
         kb-base/test-utils
         racket/class)

(check-exn
 exn:fail?
 (lambda ()
   (test-kb-program (insert 5))))

(check-exn
 exn:fail?
 (lambda ()
   (test-kb-program (do-times (count-iters (set-position 0) 1 'sexp)
                             (insert "a")))))

(check-exn
 exn:fail?
 (lambda ()
   (test-kb-program (seek-while (set-position 0) 1 'simple))))

(check-exn
 exn:fail?
 (lambda ()
   (test-kb-program (count-iters (seq (forward-sexp-exists?)
                                     (set-position 1))
                                1
                                'sexp))))

(check-exn
 exn:fail?
 (lambda ()
   (test-kb-program (kb-let (['b 4])
                     (count-iters (kb-set! 'b (set-position 1))
                                  1
                                  'sexp)))))
