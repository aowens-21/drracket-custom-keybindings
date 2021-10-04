#lang racket/base

(require racket/contract
         rackunit
         "kb-expr.rkt")

(require (for-syntax syntax/parse
                     racket/syntax
                     racket/base))

(provide (contract-out [insert (-> (or/c string? symbol? kb-expr?) kb-expr?)]
                       [delete (->* ((or/c exact-nonnegative-integer? symbol? kb-expr?))
                                    [(or/c #f exact-nonnegative-integer? symbol? kb-expr?)]
                                    kb-expr?)]
                       [insert-return (-> kb-expr?)]
                       [set-position (-> (or/c exact-nonnegative-integer? symbol? kb-expr?)
                                         kb-expr?)]
                       [move-position (-> (or/c symbol?
                                                symbol?
                                                kb-expr?)
                                          step-type?
                                          kb-expr?)]
                       [forward-sexp (-> kb-expr?)]
                       [backward-sexp (-> kb-expr?)]
                       [down-sexp (-> kb-expr?)]
                       [up-sexp (-> kb-expr?)]
                       [get-forward-sexp (-> kb-expr?)]
                       [get-position (-> kb-expr?)]
                       [last-position (-> kb-expr?)]
                       [get-character (->* ()
                                           [(or/c #f exact-nonnegative-integer? symbol? kb-expr?)]
                                           kb-expr?)]
                       [get-forward-word (->* ()
                                              [(or/c #f exact-nonnegative-integer? symbol? kb-expr?)]
                                              kb-expr?)]
                       [get-text (-> (or/c exact-nonnegative-integer? symbol? kb-expr?)
                                     (or/c exact-nonnegative-integer? symbol? kb-expr?)
                                     kb-expr?)]
                       [count-iters (-> (or/c boolean? symbol? buffer-safe-kb-expr?)
                                        (or/c number? symbol? kb-expr?)
                                        step-type?
                                        kb-expr?)]
                       [kb-set! (-> symbol?
                                    (or/c base-val?
                                          symbol?
                                          kb-expr?)
                                    kb-expr?)]
                       [kb-if (->* ((or/c boolean? symbol? kb-expr?)
                                    (or/c base-val? symbol? kb-expr?))
                                   ((or/c base-val? symbol? kb-expr?))
                                   kb-expr?)]
                       [sub (-> (or/c number? symbol? kb-expr?)
                                (or/c number? symbol? kb-expr?)
                                kb-expr?)]
                       [add (-> (or/c number? symbol? kb-expr?)
                                (or/c number? symbol? kb-expr?)
                                kb-expr?)]
                       [seek-while (-> (or/c boolean? symbol? buffer-safe-kb-expr?)
                                       (or/c number? symbol? kb-expr?)
                                       step-type?
                                       kb-expr?)]
                       [kb-not (-> (or/c boolean? symbol? kb-expr?)
                                   kb-expr?)]
                       [kb-or (-> (or/c boolean? symbol? kb-expr?)
                                  (or/c boolean? symbol? kb-expr?)
                                  kb-expr?)]
                       [kb-and (-> (or/c boolean? symbol? kb-expr?)
                                   (or/c boolean? symbol? kb-expr?)
                                   kb-expr?)]
                       [kb-gte (-> (or/c number? symbol? kb-expr?)
                                   (or/c number? symbol? kb-expr?)
                                   kb-expr?)]
                       [kb-lte (-> (or/c number? symbol? kb-expr?)
                                   (or/c number? symbol? kb-expr?)
                                   kb-expr?)]
                       [kb-gt (-> (or/c number? symbol? kb-expr?)
                                   (or/c number? symbol? kb-expr?)
                                   kb-expr?)]
                       [kb-lt (-> (or/c number? symbol? kb-expr?)
                                   (or/c number? symbol? kb-expr?)
                                   kb-expr?)]
                       [kb-equal? (-> (or/c base-val? symbol? kb-expr?)
                                      (or/c base-val? symbol? kb-expr?)
                                      kb-expr?)]
                       [forward-sexp-exists? (-> kb-expr?)]
                       [swap (-> (or/c base-val? symbol? kb-expr?)
                                 (or/c base-val? symbol? kb-expr?)
                                 (or/c base-val? symbol? kb-expr?)
                                 kb-expr?)])
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
                            (or (buffer-safe-kb-expr? expr)
                                (base-val? expr)
                                (step-type? expr)
                                (symbol? expr)))
                          (list checked-ids ...))
                  (buffer-safe-kb-expr body)]
                 [else
                  (kb-expr body)])))]
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
           (if (~? v #f)
               (buffer-safe-kb-expr body)
               (kb-expr body))))]))

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
  (-> (or/c base-val? symbol? kb-expr?)
      (or/c base-val? symbol? kb-expr?)
      kb-expr?)
  make-seq-helper)

(define-kb-op (insert s)
  `(insert ,s))

(define-kb-op (insert-return)
  `(insert-return))

(define-kb-op (delete start [end #f])
  `(delete ,start ,end))

(define-kb-op (set-position pos)
  `(set-position ,pos))

(define-kb-op (move-position code kind)
  `(move-position ,code ,kind))

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
     #:declare count (expr/c #'(or/c number? kb-expr?))
     #:declare body-expr (expr/c #'kb-expr?)
     #'(make-do-times count (seq body-expr ...))]))

(define-kb-op (make-do-times count body)
  `(do-times ,count ,body))

(define-kb-op (count-iters () (condition size type))
  `(count-iters ,condition ,size ,type))

(define-syntax (kb-let stx)
  (syntax-parse stx
    [(_ ([name val-expr]) body-expr ...)
     #:declare name (expr/c #'symbol?)
     #:declare val-expr (expr/c #'(or/c base-val? symbol? kb-expr?))
     #:declare body-expr (expr/c #'(or/c base-val? symbol? kb-expr?))
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

(define-kb-op (kb-or () (e1 e2))
  `(kb-or ,e1 ,e2))

(define-kb-op (kb-and () (e1 e2))
  `(kb-and ,e1 ,e2))

(define-kb-op (kb-gte () (e1 e2))
  `(kb-gte ,e1 ,e2))

(define-kb-op (kb-lte () (e1 e2))
  `(kb-lte ,e1 ,e2))

(define-kb-op (kb-gt () (e1 e2))
  `(kb-gt ,e1 ,e2))

(define-kb-op (kb-lt () (e1 e2))
  `(kb-lt ,e1 ,e2))

(define-kb-op (kb-equal? () (e1 e2))
  `(kb-equal? ,e1 ,e2))

(define-kb-op (forward-sexp-exists?) #:safe? #t
  `(forward-sexp-exists?))

(define (swap chars-to-swap
              src-pos
              dest-pos)
  (seq
   (kb-let (['src-pos src-pos]
            ['dest-pos dest-pos]
            ['chars-to-swap chars-to-swap]
            ['src-text (get-text 'src-pos
                                 (add 'src-pos
                                      'chars-to-swap))]
            ['dest-text (get-text 'dest-pos
                                  (add 'dest-pos
                                       'chars-to-swap))])
           (set-position 'src-pos)
           (delete 'chars-to-swap)
           (insert 'dest-text)
           (set-position 'dest-pos)
           (delete 'chars-to-swap)
           (insert 'src-text)
           (set-position 'src-pos))))

(module+ test
  (check-equal? (insert "hello")
                (kb-expr '(insert "hello")))

  (check-equal? (get-position)
                (buffer-safe-kb-expr '(get-position)))

  (check-equal? (count-iters (forward-sexp-exists?)
                             1
                             'sexp)
                (buffer-safe-kb-expr `(count-iters ,(buffer-safe-kb-expr '(forward-sexp-exists?))
                                                   1
                                                   sexp)))
  
  (check-equal? (seq (insert "hello")
                     (delete 5)
                     (set-position 0))
                #s(kb-expr (seq #s(kb-expr (insert "hello"))
                                #s(kb-expr (seq #s(kb-expr (delete 5 #f))
                                                #s(kb-expr (set-position 0))))))))