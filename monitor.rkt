#lang racket

(provide monitor
         handle)

(define (monitor send-back path the-source orig-cust)
  (define lr (make-log-receiver (current-logger)
                                'info
                                'online-check-syntax))
  (thread
   (λ ()
     (let loop ()
       (define val (sync lr))
       (printf "val: ~s\n" val)
       (match val
         [(vector level message obj name)
          (cond
            [(and (list? obj) (andmap syntax? obj))
             (with-handlers ([exn:fail?
                              (λ (exn)
                                (define sp (open-output-string))
                                (parameterize ([current-error-port sp])
                                  ((error-display-handler) (exn-message exn) exn))
                                (send-back (get-output-string sp)))])
               (for ([stx (in-list obj)])
                 (look-for-kb-props stx))
               (send-back obj))])]
         [_ (void)])
       (loop)))))

(define (handle expanded path the-source orig-cust)
  (printf "handle called\n")
  (unless (exn? expanded)
    (printf "no exn\n")
    (define kb-hash (make-hash))
    (let loop ([s expanded])
      (cond [(syntax? s)
             (when (syntax-property s 'keybinding-info)
               (extract-kb-info-prop kb-hash (syntax-property s 'keybinding-info)))
             (loop (syntax-e s))]
            [(pair? s)
             (loop (car s))
             (loop (cdr s))]))
    (printf "~s\n" kb-hash)
    kb-hash))

(define (extract-kb-info-prop kbs prop)
  (define keystroke (vector-ref prop 0))
  (define program (vector-ref prop 1))
  (define name (vector-ref prop 2))
  (define active-range (vector-ref prop 3))
  (hash-set! kbs keystroke (vector (lambda () program) name active-range)))

(define (look-for-kb-props stx)
  (printf "got called\n")
  (let loop ([prop (syntax-property stx 'keybinding-info)])
    (cond [(pair? prop)
           (loop (car prop))
           (loop (cdr prop))]
          [(vector? prop)
           (printf "found a keybinding\n")])))
