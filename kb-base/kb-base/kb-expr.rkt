#lang racket/base

(require racket/match)

(provide (struct-out kb-expr)
         (struct-out buffer-safe-kb-expr)
         step-type?
         base-val?
         well-formed-kb-base-program?)

(struct kb-expr (inner-expr) #:prefab)
(struct buffer-safe-kb-expr kb-expr () #:prefab)

(define (base-val? e)
  (or (number? e)
      (string? e)
      (char? e)
      (boolean? e)))

(define (step-type? t)
  (if (member t '(simple
                  line
                  page
                  word
                  sexp))
      #t
      #f))

(define (well-formed-kb-base-program? p)
  (match p
    [(? base-val? p) #t]
    [(kb-expr inner-expr)
     (match inner-expr
       [`(get-position) #t]
       [`(last-position) #t]
       [`(forward-sexp) #t]
       [`(backward-sexp) #t]
       [`(up-sexp) #t]
       [`(down-sexp) #t]
       [`(insert-return) #t]
       [`(forward-sexp-exists?) #t]
       [`(get-forward-sexp) #t]
       [`(insert ,expr)
        (well-formed-kb-base-program? expr)]
       [`(delete ,expr #f)
        (well-formed-kb-base-program? expr)]
       [`(delete ,start-expr ,end-expr)
        (and (well-formed-kb-base-program? start-expr)
             (well-formed-kb-base-program? end-expr))]
       [`(set-position ,expr)
        (well-formed-kb-base-program? expr)]
       [`(seq ,expr1 ,expr2)
        (and (well-formed-kb-base-program? expr1)
             (well-formed-kb-base-program? expr2))]
       [`(move-position ,size-expr ,type)
        (and (well-formed-kb-base-program? size-expr)
             (step-type? type))]
       [`(get-character #f) #t]
       [`(get-character ,pos-expr)
        (well-formed-kb-base-program? pos-expr)]
       [`(get-forward-word #f) #t]
       [`(get-forward-word ,pos-expr)
        (well-formed-kb-base-program? pos-expr)]
       [`(get-text ,start-expr ,end-expr)
        (and (well-formed-kb-base-program? start-expr)
             (well-formed-kb-base-program? end-expr))]
       [`(do-times ,count-expr ,body-expr)
        (and (well-formed-kb-base-program? count-expr)
             (well-formed-kb-base-program? body-expr))]
       [`(count-iters ,cond-expr ,size-expr ,type)
        (and (well-formed-buffer-safe-kb-expr? cond-expr)
             (well-formed-kb-base-program? size-expr)
             (step-type? type))]
       [`(seek-while ,cond-expr ,size-expr ,type)
        (and (well-formed-buffer-safe-kb-expr? cond-expr)
             (well-formed-kb-base-program? size-expr)
             (step-type? type))]
       [`(kb-let ,name ,val-expr ,body-expr)
        (and (symbol? name)
             (well-formed-kb-base-program? val-expr)
             (well-formed-kb-base-program? body-expr))]
       [`(kb-set! ,name ,val-expr)
        (and (symbol? name)
             (well-formed-kb-base-program? val-expr))]
       [`(kb-if ,cond-expr ,thn-expr ,els-expr)
        (and (well-formed-kb-base-program? cond-expr)
             (well-formed-kb-base-program? thn-expr)
             (well-formed-kb-base-program? els-expr))]
       [`(kb-equal? ,expr1 ,expr2)
        (and (well-formed-kb-base-program? expr1)
             (well-formed-kb-base-program? expr2))]
       [`(kb-not ,expr)
        (well-formed-kb-base-program? expr)]
       [`(kb-or ,e1 ,e2)
        (and (well-formed-kb-base-program? e1)
             (well-formed-kb-base-program? e2))]
       [`(add ,expr1 ,expr2)
        (and (well-formed-kb-base-program? expr1)
             (well-formed-kb-base-program? expr2))]
       [`(sub ,expr1 ,expr2)
        (and (well-formed-kb-base-program? expr1)
             (well-formed-kb-base-program? expr2))]
       [else #f])]
    [(? symbol? p) #t]
    [else #f]))

(define (well-formed-buffer-safe-kb-expr? e)
  (match e
    [(? base-val?) #t]
    [(buffer-safe-kb-expr inner-expr)
     (match inner-expr
       [`(get-position) #t]
       [`(get-text ,start ,end)
        (and (well-formed-buffer-safe-kb-expr? start)
             (well-formed-buffer-safe-kb-expr? end))]
       [`(last-position) #t]
       [`(get-character ,pos)
        (if pos
            (well-formed-buffer-safe-kb-expr? pos)
            #t)]
       [`(get-forward-word ,pos)
        (if pos
            (well-formed-buffer-safe-kb-expr? pos)
            #t)]
       [`(get-forward-sexp) #t]
       [`(forward-sexp-exists?) #t]
       [`(kb-equal? ,e1 ,e2)
        (and (well-formed-buffer-safe-kb-expr? e1)
             (well-formed-buffer-safe-kb-expr? e2))]
       [`(kb-not ,e1)
        (well-formed-buffer-safe-kb-expr? e1)]
       [`(kb-or ,e1 ,e2)
        (and (well-formed-buffer-safe-kb-expr? e1)
             (well-formed-buffer-safe-kb-expr? e2))]
       [`(add ,n1 ,n2)
        (and (well-formed-buffer-safe-kb-expr? n1)
             (well-formed-buffer-safe-kb-expr? n2))]
       [`(sub ,n1 ,n2)
        (and (well-formed-buffer-safe-kb-expr? n1)
             (well-formed-buffer-safe-kb-expr? n2))]
       [`(kb-if ,condition ,thn ,els)
        (and (well-formed-buffer-safe-kb-expr? condition)
             (well-formed-buffer-safe-kb-expr? thn)
             (well-formed-buffer-safe-kb-expr? els))]
       [`(kb-let ,name ,val-expr ,body)
        (and (well-formed-buffer-safe-kb-expr? val-expr)
             (well-formed-buffer-safe-kb-expr? body))]
       [`(seq ,e1 ,e2)
        (and (well-formed-buffer-safe-kb-expr? e1)
             (well-formed-buffer-safe-kb-expr? e2))]
       [`(kb-set! ,name ,new-val-expr)
        (well-formed-buffer-safe-kb-expr? new-val-expr)]
       [`(count-iters ,condition ,step-size ,step-type)
        (and (well-formed-buffer-safe-kb-expr? condition)
             (well-formed-buffer-safe-kb-expr? step-size))]
       [else #f])]
    [(? symbol?) #t]
    [else #f]))