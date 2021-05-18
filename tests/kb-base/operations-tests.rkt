#lang racket/base

(require rackunit
         racket/class
         kb-base
         "test-utils.rkt")

(test-kb-program
 (insert "hello")
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 5)
   (check-equal? result-text "hello")))
                 

(test-kb-program
 (seq (insert "hello")
      (insert ", world!"))
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 13)
   (check-equal? result-text "hello, world!")))

(test-kb-program
 (delete 0 28)
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 8)
   (check-equal? result-text "deleted."))
 #:setup-proc (lambda (editor)
                (send editor insert "Some of these words will be deleted.")))

(test-kb-program
 (delete 5)
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 0)
   (check-equal? result-text ", world!"))
 #:setup-proc (lambda (editor)
                (send editor insert "Hello, world!")
                (send editor set-position 0)))

(test-kb-program
 (insert-return)
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 2)
   (check-equal? result-text "a\nb"))
 #:setup-proc (lambda (editor)
                (send editor insert "ab")
                (send editor set-position 1)))

(test-kb-program
 (set-position 0)
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 0)
   (check-equal? result-text "")))

(test-kb-program
 (set-position 5)
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 0)
   (check-equal? result-text "")))

(test-kb-program
 (set-position 3)
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 3)
   (check-equal? result-text "abcde"))
 #:setup-proc (lambda (editor)
                (send editor insert "abcde")))

(test-kb-program
 (seq (move-position 2 'simple)
      (move-position -1 'simple))
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 1)
   (check-equal? result-text "abc"))
 #:setup-proc (lambda (editor)
                (send editor insert "abc")
                (send editor set-position 0)))

(test-kb-program
 (forward-sexp)
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 5)
   (check-equal? result-text "(abc)"))
 #:setup-proc (lambda (editor)
                (send editor insert "(abc)")
                (send editor set-position 0)))

(test-kb-program
 (backward-sexp)
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 0)
   (check-equal? result-text "(abc)"))
 #:setup-proc (lambda (editor)
                (send editor insert "(abc)")))

(test-kb-program
 (down-sexp)
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 1)
   (check-equal? result-text "(abc)"))
 #:setup-proc (lambda (editor)
                (send editor insert "(abc)")
                (send editor set-position 0)))
(test-kb-program
 (up-sexp)
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 0)
   (check-equal? result-text "(abc)"))
 #:setup-proc (lambda (editor)
                (send editor insert "(abc)")
                (send editor set-position 0)
                (send editor down-sexp (send editor get-end-position))))

(test-kb-program
 (get-forward-sexp)
 (lambda (result-val result-pos result-text)
   (check-equal? result-val 3)
   (check-equal? result-pos 0)
   (check-equal? result-text "abc()"))
 #:setup-proc (lambda (editor)
                (send editor insert "abc()")
                (send editor set-position 0)))

(test-kb-program
 (get-character)
 (lambda (result-val result-pos result-text)
   (check-equal? result-val #\nul)
   (check-equal? result-pos 0)
   (check-equal? result-text "")))

(test-kb-program
 (get-character)
 (lambda (result-val result-pos result-text)
   (check-equal? result-val #\a)
   (check-equal? result-pos 0)
   (check-equal? result-text "abc"))
 #:setup-proc (lambda (editor)
                (send editor insert "abc")
                (send editor set-position 0)))

(test-kb-program
 (get-character 1)
 (lambda (result-val result-pos result-text)
   (check-equal? result-val #\b)
   (check-equal? result-pos 0)
   (check-equal? result-text "abc"))
 #:setup-proc (lambda (editor)
                (send editor insert "abc")
                (send editor set-position 0)))

(test-kb-program
 (last-position)
 (lambda (result-val result-pos result-text)
   (check-equal? result-val 10)
   (check-equal? result-pos 10)
   (check-equal? result-text "abcdefghij"))
 #:setup-proc (lambda (editor)
                (send editor insert "abcdefghij")))

(test-kb-program
 (get-position)
 (lambda (result-val result-pos result-text)
   (check-equal? result-val 2)
   (check-equal? result-pos 2)
   (check-equal? result-text "abc"))
 #:setup-proc (lambda (editor)
                (send editor insert "abc")
                (send editor set-position 2)))

(test-kb-program
 (get-forward-word)
 (lambda (result-val result-pos result-text)
   (check-equal? result-val "hello")
   (check-equal? result-pos 0)
   (check-equal? result-text "hello world"))
 #:setup-proc (lambda (editor)
                (send editor insert "hello world")
                (send editor set-position 0)))

(test-kb-program
 (get-forward-word 5)
 (lambda (result-val result-pos result-text)
   (check-equal? result-val "world")
   (check-equal? result-pos 0)
   (check-equal? result-text "hello world"))
 #:setup-proc (lambda (editor)
                (send editor insert "hello world")
                (send editor set-position 0)))

(test-kb-program
 (get-text 1 6)
 (lambda (result-val result-pos result-text)
   (check-equal? result-val "t's a")
   (check-equal? result-pos 20)
   (check-equal? result-text "it's a me, test text"))
 #:setup-proc (lambda (editor)
                (send editor insert "it's a me, test text")))

(test-kb-program
 (do-times 3 (insert "hello"))
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 15)
   (check-equal? result-text "hellohellohello")))

(test-kb-program
 (do-times (+ 1 2) (insert "hello"))
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 15)
   (check-equal? result-text "hellohellohello")))

(test-kb-program
 (count-iters (forward-sexp-exists?) 1 'sexp)
 (lambda (result-val result-pos result-text)
   (check-equal? result-val 3)
   (check-equal? result-pos 0)
   (check-equal? result-text "(a) (b) (c)"))
 #:setup-proc (lambda (editor)
                (send editor insert "(a) (b) (c)")
                (send editor set-position 0)))

(test-kb-program
 (do-times (count-iters (forward-sexp-exists?) 1 'sexp)
           (seq (insert "a")
                (insert "b")))
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 6)
   (check-equal? result-text "ababab(a) (b) (c)"))
 #:setup-proc (lambda (editor)
                (send editor insert "(a) (b) (c)")
                (send editor set-position 0)))

(test-kb-program
 (kb-let (['iters (count-iters (forward-sexp-exists?) 1 'sexp)])
         (do-times 'iters
                   (seq (insert "a")
                        (insert "b"))))
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 6)
   (check-equal? result-text "ababab(a) (b) (c)"))
 #:setup-proc (lambda (editor)
                (send editor insert "(a) (b) (c)")
                (send editor set-position 0)))

(test-kb-program
 (kb-let (['a 4])
         (seq (kb-set! 'a 1)
              (do-times 'a (insert "a"))))
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 1)
   (check-equal? result-text "a")))

(test-kb-program
 (kb-let (['b 10])
         (kb-equal? (seq (kb-set! 'b 5)
                         5)
                    'b))
 (lambda (result-val result-pos result-text)
   (check-equal? result-val #t)
   (check-equal? result-pos 0)
   (check-equal? result-text "")))

(test-kb-program
 (kb-let (['a 4])
         (seq (kb-set! 'a 1)
              (seq (kb-set! 'a 2)
                   (do-times 'a (insert "a")))))
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 2)
   (check-equal? result-text "aa")))

(test-kb-program
 (kb-if (forward-sexp-exists?)
        (insert "there is an sexp")
        (insert "there is not an sexp"))
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 20)
   (check-equal? result-text "there is not an sexp")))

(test-kb-program
 (kb-if (forward-sexp-exists?)
        (insert "there is an sexp")
        (insert "there is not an sexp"))
 (lambda (result-val result-pos result-text)
   (check-equal? result-pos 16)
   (check-equal? result-text "there is an sexp()"))
 #:setup-proc (lambda (editor)
                (send editor insert "()")
                (send editor set-position 0)))

(test-kb-program
 (kb-if #f
        (insert "was true"))
 (lambda (result-val result-pos result-text)
   (check-equal? result-text "")
   (check-equal? result-pos 0)))