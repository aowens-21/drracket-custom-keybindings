#lang racket/base

(require rackunit
         "../../kb-base/kb-base/kb-expr.rkt")

(check-equal? (well-formed-kb-base-program? #s(kb-expr (insert "a")))
              #t)

(check-equal? (well-formed-kb-base-program? #s(kb-expr (seq #s(kb-expr (insert "b"))
                                                            #s(kb-expr (seq #s(kb-expr (delete 5 10))
                                                                            #s(kb-expr (set-position 3)))))))
              #t)

(check-equal? (well-formed-kb-base-program? #s((buffer-safe-kb-expr kb-expr 1)
                                               (count-iters #s((buffer-safe-kb-expr kb-expr 1)
                                                               (set-position 1)) 1 sexp)))
              #f)

(check-equal? (well-formed-kb-base-program? #s((buffer-safe-kb-expr kb-expr 1)
                                               (count-iters #s((buffer-safe-kb-expr kb-expr 1)
                                                               (get-position)) 1 sexp)))
              #t)            