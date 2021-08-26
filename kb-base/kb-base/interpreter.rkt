#lang racket/base

(require racket/match
         racket/class
         "operations.rkt"
         "kb-expr.rkt")

(provide run-kb-program
         interp)

(define (run-kb-program program editor)
  (interp program editor (make-hash)))

(define (interp expr editor bindings)
  (match expr
    [(? base-val?) expr]
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
       [`(move-position ,code ,kind)
        (do-move-position editor code kind)]
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
       [`(kb-or ,e1 ,e2)
        (define e1-val (interp e1 editor bindings))
        (define e2-val (interp e2 editor bindings))
        (error-unless-boolean e1-val)
        (error-unless-boolean e2-val)
        (or e1-val e2-val)]
       [`(kb-and ,e1 ,e2)
        (define e1-val (interp e1 editor bindings))
        (define e2-val (interp e2 editor bindings))
        (error-unless-boolean e1-val)
        (error-unless-boolean e2-val)
        (and e1-val e2-val)]
       [`(kb-gte ,e1 ,e2)
        (num-op >=
                (interp e1 editor bindings)
                (interp e2 editor bindings))]
       [`(kb-lte ,e1 ,e2)
        (num-op <=
                (interp e1 editor bindings)
                (interp e2 editor bindings))]
       [`(kb-gt ,e1 ,e2)
        (num-op >
                (interp e1 editor bindings)
                (interp e2 editor bindings))]
       [`(kb-lt ,e1 ,e2)
        (num-op <
                (interp e1 editor bindings)
                (interp e2 editor bindings))]
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
    [(? symbol?)
     (lookup expr bindings)]
    [else (error 'interp "Unexpected expr: ~a" expr)]))

(define (error-unless-number val)
  (unless (number? val)
    (raise-argument-error 'interp "number?" val)))

(define (error-unless-string val)
  (unless (string? val)
    (raise-argument-error 'interp "string?" val)))

(define (error-unless-boolean val)
  (unless (boolean? val)
    (raise-argument-error 'interp "boolean?" val)))

(define (num-op op n1 n2)
  (error-unless-number n1)
  (error-unless-number n2)
  (op n1 n2))

(define (lookup id bindings)
  (define val/#f (if (hash-has-key? bindings id)
                     (hash-ref bindings id)
                     #f))
  (unless val/#f
    (error 'interp "Unbound identifier: ~s" id))
  val/#f)

(define (do-move-position editor code kind)
  (cond [(equal? kind 'sexp)
         (if (equal? code 'left)
             (send editor backward-sexp (send editor get-end-position))
             (send editor forward-sexp (send editor get-end-position)))]
        [else
         (send editor move-position code #f kind)]))

(define (process-get-forward-word editor pos)
  (define old-start (send editor get-start-position))
  (define old-end (send editor get-end-position))
  (send editor set-position pos)
  (let move-forward ()
    (send editor move-position 'right #f 'word)
    (if (or (= (send editor last-position) (send editor get-end-position))
            (not (char=? (send editor get-character (send editor get-end-position)) #\-)))
        (begin0 (send editor get-word-at (send editor get-end-position))
                (send editor set-position old-start old-end))
        (move-forward))))

(define (do-count-iters editor condition bindings step-size step-type)
  (let ([iter-count 0]
        [original-pos (send editor get-end-position)])
    (let loop ([c (interp condition editor bindings)])
      (when c
        (define old-pos (send editor get-end-position))
        (do-step editor step-size step-type)
        (when (not (= old-pos (send editor get-end-position)))
          (set! iter-count (add1 iter-count))
          (loop (interp condition editor bindings)))))
    (send editor set-position original-pos)
    iter-count))

(define (do-step editor size type)
  (unless (not (= size 0))
    (error 'count-iters "Step size for iteration must be non-zero"))
  (define direction (cond [(member type '(page))
                           (if (< size 0) 'up 'down)]
                          [(member type '(simple word line sexp))
                           (if (< size 0) 'left 'right)]
                          [else
                           (error 'count-iters "Invalid type passed to count-iters")]))
  (define abs-size (abs size))
  (for ([i (in-range abs-size)])
    (case type
      ['sexp (if (equal? direction 'left)
                 (send editor backward-sexp (send editor get-end-position))
                 (send editor forward-sexp (send editor get-end-position)))]
      [else (send editor move-position direction #f type)])))