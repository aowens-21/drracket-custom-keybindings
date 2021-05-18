#lang racket/base

(require racket/contract
         "kb-expr.rkt")

(require (for-syntax syntax/parse
                     racket/syntax
                     racket/base))

(provide (contract-out [insert (-> is-kb-expr? is-kb-expr?)]
                       [delete (->* (is-kb-expr?)
                                    [(or/c #f is-kb-expr?)]
                                    is-kb-expr?)]
                       [insert-return (-> is-kb-expr?)]
                       [set-position (-> is-kb-expr? is-kb-expr?)]
                       [move-position (-> is-kb-expr? step-type? is-kb-expr?)]
                       [forward-sexp (-> is-kb-expr?)]
                       [backward-sexp (-> is-kb-expr?)]
                       [down-sexp (-> is-kb-expr?)]
                       [up-sexp (-> is-kb-expr?)]
                       [get-forward-sexp (-> is-kb-expr?)]
                       [get-position (-> is-kb-expr?)]
                       [last-position (-> is-kb-expr?)]
                       [get-character (->* ()
                                           [(or/c #f is-kb-expr?)]
                                           is-kb-expr?)]
                       [get-forward-word (->* ()
                                              [(or/c #f is-kb-expr?)]
                                              is-kb-expr?)]
                       [get-text (-> is-kb-expr? is-kb-expr? is-kb-expr?)]
                       [count-iters (-> is-buffer-safe-kb-expr? is-kb-expr? step-type? is-kb-expr?)]
                       [kb-set! (-> symbol? is-kb-expr? is-kb-expr?)]
                       [kb-if (->* (is-kb-expr?
                                    is-kb-expr?)
                                   ((or/c #f is-kb-expr?))
                                   is-kb-expr?)]
                       [sub (-> is-kb-expr? is-kb-expr? is-kb-expr?)]
                       [add (-> is-kb-expr? is-kb-expr? is-kb-expr?)]
                       [seek-while (-> is-buffer-safe-kb-expr? is-kb-expr? step-type? is-kb-expr?)]
                       [kb-not (-> is-kb-expr? is-kb-expr?)]
                       [kb-equal? (-> is-kb-expr? is-kb-expr? is-kb-expr?)]
                       [forward-sexp-exists? (-> is-kb-expr?)])
         seq
         do-times
         kb-let)

(define-syntax (define-kb-op stx)
  (syntax-parse stx
    ;; Given a list of args to check and args not to check,
    ;; generate a define that does the checking and if is-buffer-safe-kb-expr? checks
    ;; pass for all checked-args, we generate a buffer-safe-kb-expr
    [(_ (name (unchecked-args ...)
              (checked-args ...))
        body)
     (with-syntax ([name-id (format-id #'name "~a" (syntax-e #'name))]
                   [(unchecked-ids ...) (map (lambda (arg)
                                               (syntax-parse arg
                                                 [(uncheck-id uncheck-val)
                                                  #'uncheck-id]
                                                 [uncheck-id #'uncheck-id]))
                                             (syntax->list #'(unchecked-args ...)))]
                   [(checked-ids ...) (map (lambda (arg)
                                             (syntax-parse arg
                                               [(check-id check-val)
                                                #'check-id]
                                               [check-id #'check-id]))
                                           (syntax->list #'(checked-args ...)))])
       #'(define (name-id unchecked-args ... checked-args ...)
           (cond [(andmap (lambda (expr)
                            (or (is-buffer-safe-kb-expr? expr)
                                (base-val? expr)
                                (step-type? expr)
                                (symbol? expr)))
                          (list checked-ids ...))
                  (let ([unchecked-ids (if (kb-expr? unchecked-ids)
                                           (kb-expr-inner-expr unchecked-ids)
                                           unchecked-ids)]
                        ...
                        [checked-ids (if (kb-expr? checked-ids)
                                         (kb-expr-inner-expr checked-ids)
                                         checked-ids)]
                        ...)
                    (buffer-safe-kb-expr body))]
                 [else
                  (let ([unchecked-ids (if (kb-expr? unchecked-ids)
                                           (kb-expr-inner-expr unchecked-ids)
                                           unchecked-ids)]
                        ...
                        [checked-ids (if (kb-expr? checked-ids)
                                         (kb-expr-inner-expr checked-ids)
                                         checked-ids)]
                        ...)
                    (kb-expr body))])))]
    ;; Generating a define but won't check its arguments,
    ;; determines if an op should produce safe based on #:safe?
    [(_ (name args ...) (~optional (~seq #:safe? v)) body)
     (with-syntax ([name-id (format-id #'name "~a" (syntax-e #'name))]
                   [(arg-ids ...) (map (lambda (arg)
                                         (syntax-parse arg
                                           [(check-id check-val)
                                            #'check-id]
                                           [check-id #'check-id]))
                                       (syntax->list #'(args ...)))])
       #'(define (name-id args ...)
           (let ([arg-ids (if (kb-expr? arg-ids)
                              (kb-expr-inner-expr arg-ids)
                              arg-ids)]
                 ...)
             (if (~? v #f)
                 (buffer-safe-kb-expr body)
                 (kb-expr body)))))]))

(define-syntax (seq stx)
  (syntax-parse stx
    [(_ e1 e2 e3 ...+)
     #'(make-seq e1 (seq e2 e3 ...))]
    [(_ e1 e2)
     #'(make-seq e1 e2)]
    [(_ e1)
     #'(make-seq e1 #f)]))

(define-kb-op (make-seq-helper e1 e2)
  `(seq ,e1 ,e2))

(define/contract make-seq
  (-> is-kb-expr? is-kb-expr? is-kb-expr?)
  make-seq-helper)

(define-kb-op (insert s)
  `(insert ,s))

(define-kb-op (insert-return)
  `(insert-return))

(define-kb-op (delete start [end #f])
  `(delete ,start ,end))

(define-kb-op (set-position pos)
  `(set-position ,pos))

(define-kb-op (move-position size type)
  `(move-position ,size ,type))

(define-kb-op (forward-sexp)
  `(forward-sexp))

(define-kb-op (backward-sexp)
  `(backward-sexp))

(define-kb-op (down-sexp)
  `(down-sexp))

(define-kb-op (up-sexp)
  `(up-sexp))

(define-kb-op (get-character () ([pos #f]))
  `(get-character ,pos))

(define-kb-op (get-forward-sexp) #:safe? #t
  `(get-forward-sexp))

(define-kb-op (last-position) #:safe? #t
  `(last-position))

(define-kb-op (get-position) #:safe? #t
  `(get-position))

(define-kb-op (get-forward-word () ([pos #f]))
  `(get-forward-word ,pos))

(define-kb-op (get-text () (start end))
  `(get-text ,start ,end))

(define-syntax (do-times stx)
  (syntax-parse stx
    [(_ count body-expr ...)
     #:declare count (expr/c #'is-kb-expr?)
     #:declare body-expr (expr/c #'is-kb-expr?)
     #'(make-do-times count (seq body-expr ...))]))

(define-kb-op (make-do-times count body)
  `(do-times ,count ,body))

(define-kb-op (count-iters () (condition size type))
  `(count-iters ,condition ,size ,type))

(define-syntax (kb-let stx)
  (syntax-parse stx
    [(_ ([name val-expr]) body-expr ...)
     #:declare name (expr/c #'symbol?)
     #:declare val-expr (expr/c #'is-kb-expr?)
     #:declare body-expr (expr/c #'is-kb-expr?)
     #'(do-kb-let name val-expr (seq body-expr ...))]
    [(_ ([name val-expr] let-clause ...+) body-expr ...)
     #'(do-kb-let name val-expr (kb-let (let-clause ...) body-expr ...))]))

(define-kb-op (do-kb-let (name) (val body))
  `(kb-let ,name ,val ,body))

(define-kb-op (kb-set! (name) (new-val))
  `(kb-set! ,name ,new-val))

(define-kb-op (kb-if () (condition thn [els #f]))
  `(kb-if ,condition ,thn ,els))

(define-kb-op (sub () (n1 n2))
  `(sub ,n1 ,n2))

(define-kb-op (add () (n1 n2))
  `(add ,n1 ,n2))

(define-kb-op (seek-while condition size type)
  `(seek-while ,condition ,size ,type))

(define-kb-op (kb-not () (e))
  `(kb-not ,e))

(define-kb-op (kb-equal? () (e1 e2))
  `(kb-equal? ,e1 ,e2))

(define-kb-op (forward-sexp-exists?) #:safe? #t
  `(forward-sexp-exists?))