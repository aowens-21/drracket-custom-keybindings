#lang racket

(provide handle)

(require "../kb-base/kb-base/kb-expr.rkt"
         rackunit)

(define (handle expanded path the-source orig-cust)
  (unless (exn? expanded)
    (with-handlers ([exn:fail? (λ (e) (printf "~s\n" e))])
      (define kb-hash (make-hash))
      (define registered-kb-names (mutable-set))
      (get-extensions-from-stx expanded
                               (λ (stx)
                                 (extract-kb kb-hash registered-kb-names stx)))
      kb-hash)))

(define (get-extensions-from-stx stx extract-extension-func)
  (let loop ([s stx])
    (cond [(syntax? s)
           (when (and (syntax-property s 'keybinding-info)
                      (syntax-property s 'origin))
             (define origin (syntax-property s 'origin))
             (define actual-origin (let loop ([l origin])
                                     (cond [(list? l)
                                            (if (list? (first l))
                                                (loop (first l))
                                                (last l))])))
             ;; Using the origin property check to make sure it's the original syntax,
             ;; this + source locations is the closest approximation to "is it in the buffer"
             ;; I've found
             (when (syntax-original? actual-origin)
               (define kb-info (syntax-property s 'keybinding-info))
               (define kb
                 (cond [(pair? kb-info)
                        (pair-last kb-info)]
                       [else
                        kb-info]))
               (define kb-srcloc (vector-ref kb 4))
               (when (and (equal? (srcloc-source kb-srcloc) (syntax-source actual-origin))
                          (= (srcloc-position kb-srcloc) (syntax-position actual-origin)))
                 (extract-extension-func s))))
           (loop (syntax-e s))]
          [(pair? s)
           (loop (car s))
           (loop (cdr s))])))

(define (valid-kb-vec? kb)
  (and (vector? kb)
       (= (vector-length kb) 5) 
       (string? (vector-ref kb 0))
       #t ;; TODO: check for the struct type
       (string? (vector-ref kb 2))
       (or (symbol? vector-ref kb 3)
           (pair? vector-ref kb 3))
       (srcloc? (vector-ref kb 4))))

(define (extract-kb kb-table registered-kb-names stx)
  (define kb-vec/list (syntax-property stx 'keybinding-info))
  (define kb
    (cond [(pair? kb-vec/list)
           (pair-last kb-vec/list)]
          [else
           kb-vec/list]))
  (when (valid-kb-vec? kb)
    (unless (set-member? registered-kb-names (vector-ref kb 2))
      (set-add! registered-kb-names (vector-ref kb 2))
      ;; extract the actual quoted keybinding program from the struct
      (vector-set! kb 1 (vector-ref (struct->vector (vector-ref kb 1)) 1))
      ;; set up the correct active ranges in the editor buffer
      (vector-set! kb 3 (convert-range-type-to-positions (vector-ref kb 3)
                                                         (vector-ref kb 4)))
      (when kb
        (if (hash-has-key? kb-table (vector-ref kb 0))
            (hash-update! kb-table
                          (vector-ref kb 0)
                          (lambda (old-val)
                            (append (if (not (list? old-val))
                                        (list old-val)
                                        old-val)
                                    (list (vector-drop-right kb 1)))))
            (hash-set! kb-table
                       (vector-ref kb 0)
                       (vector-drop-right kb 1)))))))

(define/contract (convert-range-type-to-positions range-type sloc)
  (-> (or/c symbol?
            (cons/c number? number?))
      srcloc?
      (or/c symbol?
            (cons/c number? number?)))
  (cond [(pair? range-type)
         range-type]
        [(equal? range-type 'global)
         'global]
        [(equal? range-type 'local)
         (cons (srcloc-position sloc)
               (+ (srcloc-position sloc)
                  (srcloc-span sloc)))]
        [else (error 'extract-kb "Invalid range-type for keybinding: ~a" range-type)]))

(define (pair-last pair)
  (if (pair? pair)
      (pair-last (cdr pair))
      pair))

(module+ test
  (check-equal? (convert-range-type-to-positions 'global
                                                 (make-srcloc #f #f #f #f #f))
                'global)
  (check-equal? (convert-range-type-to-positions 'local
                                                 (make-srcloc 'dummy-source
                                                              5
                                                              1
                                                              42
                                                              100))
                (cons 42 142))
  (check-exn exn:fail?
             (λ ()
               (convert-range-type-to-positions 'weird-symbol
                                                (make-srcloc 'dummy-source
                                                             1
                                                             1
                                                             1
                                                             1)))))