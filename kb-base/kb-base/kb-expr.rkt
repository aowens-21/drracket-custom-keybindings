#lang racket/base

(require racket/match)

(provide (struct-out kb-expr)
         (struct-out buffer-safe-kb-expr)
         base-val?
         step-type?
         is-kb-expr?
         is-buffer-safe-kb-expr?)

(struct kb-expr (inner-expr) #:prefab)
(struct buffer-safe-kb-expr kb-expr () #:prefab)

(define (base-val? e)
  (or (number? e)
      (string? e)
      (char? e)
      (boolean? e)))

(define (step-type? t)
  (member t '(simple
              line
              page
              word
              sexp)))

(define (is-kb-expr? e)
  (or (kb-expr? e)
      (base-val? e)
      (step-type? e)
      (symbol? e)))

(define (is-buffer-safe-kb-expr? e)
  (or (buffer-safe-kb-expr? e)
      (base-val? e)
      (step-type? e)
      (symbol? e)))

(define (well-formed-kb-base-program? p)
  (match p
    [(? base-val p) #t]
    [(kb-expr inner-expr)
     (match inner-expr
       [`(insert ,s)
        (define text (interp s editor bindings))
        (error-unless-string text)
        (send editor insert text)]
       [`(seq ,e1 ,e2)
        (define e1-val (interp e1 editor bindings))
        (if e2
            (interp e2 editor bindings)
            e1-val)]
       [`(delete ,range #f)
        (define range-val (interp range editor bindings))
        (error-unless-number range-val)
        (define current-pos (send editor get-end-position))
        (send editor delete current-pos (+ current-pos range-val))]
       [`(delete ,start ,end)
        (define start-val (interp start editor bindings))
        (error-unless-number start-val)
        (define end-val (interp end editor bindings))
        (error-unless-number end-val)
        (send editor delete start-val end-val)]
       [`(insert-return)
        (send editor insert-return)]
       [`(set-position ,p)
        (define pos (interp p editor bindings))
        (error-unless-number pos)
        (send editor set-position pos)]
       [`(move-position ,size ,type)
        (define step-size (interp size editor bindings))
        (error-unless-number step-size)
        (do-step editor step-size type)]
       [`(forward-sexp)
        (send editor forward-sexp (send editor get-end-position))]
       [`(backward-sexp)
        (send editor backward-sexp (send editor get-end-position))]
       [`(down-sexp)
        (send editor down-sexp (send editor get-end-position))]
       [`(up-sexp)
        (send editor up-sexp (send editor get-end-position))]
       [`(do-times ,count ,body)
        (define iter-count (interp count editor bindings))
        (error-unless-number iter-count)
        (for ([i (in-range iter-count)])
          (interp body editor bindings))]
       [`(kb-let ,name ,val-expr ,body)
        (define old-binding/#f (if (hash-has-key? bindings name)
                                   (hash-ref bindings name)
                                   #f))
        (define new-val (interp val-expr editor bindings))
        (hash-set! bindings name new-val)
        (begin0 (interp body editor bindings)
                (hash-set! bindings name old-binding/#f))]
       [`(kb-set! ,name ,new-val)
        (unless (hash-has-key? bindings name)
          (error 'interp "Cannot call set on unbound identifier: ~s" name))
        (hash-set! bindings name (interp new-val editor bindings))]
       [`(kb-if ,condition ,thn ,els)
        (define cond-result (interp condition editor bindings))
        (if cond-result
            (interp thn editor bindings)
            (when els
              (interp els editor bindings)))]
       [`(seek-while ,condition ,size ,type)
        (let loop ([c (interp condition editor bindings)])
          (when c
            (define old-pos (send editor get-end-position))
            (do-step editor size type)
            (when (not (= old-pos (send editor get-end-position)))
              (loop (interp condition editor bindings)))))]
       [`(sub ,n1 ,n2)
        (num-op - (interp n1 editor bindings) (interp n2 editor bindings))]
       [`(add ,n1 ,n2)
        (num-op + (interp n1 editor bindings) (interp n2 editor bindings))]
       [`(count-iters ,condition ,size ,type)
        (define step-size (interp size editor bindings))
        (error-unless-number step-size)
        (do-count-iters editor condition bindings step-size type)]
       [`(get-position)
        (send editor get-end-position)]
       [`(last-position)
        (send editor last-position)]
       [`(get-forward-sexp)
        (send editor get-forward-sexp (send editor get-end-position))]
       [`(kb-not ,e)
        (define expr-val (interp e editor bindings))
        (error-unless-boolean expr-val)
        (not expr-val)]
       [`(kb-equal? ,e1 ,e2)
        (equal? (interp e1 editor bindings)
                (interp e2 editor bindings))]
       [`(forward-sexp-exists?)
        (if (send editor get-forward-sexp (send editor get-end-position))
            #t
            #f)]
       [`(get-forward-word #f)
        (process-get-forward-word editor (send editor get-end-position))]
       [`(get-forward-word ,pos)
        (define pos-val (interp pos editor bindings))
        (error-unless-number pos-val)
        (process-get-forward-word editor pos-val)]
       [`(get-text ,start ,end)
        (define start-val (interp start editor bindings))
        (define end-val (interp end editor bindings))
        (error-unless-number start-val)
        (error-unless-number end-val)
        (send editor get-text start-val end-val)]
       [`(get-character #f)
        (send editor get-character (send editor get-end-position))]
       [`(get-character ,pos)
        (define pos-val (interp pos editor bindings))
        (error-unless-number pos-val)
        (send editor get-character (interp pos editor bindings))])]
     [(? symbol? p) #t]))