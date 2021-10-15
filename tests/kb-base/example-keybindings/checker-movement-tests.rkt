#lang racket/base

(require syntax/parse
         "checkers-movement.rkt"
         kb-base/test-utils
         rackunit
         racket/class
         kb-base
         (for-syntax racket/base
                     racket/list
                     syntax/parse
                     kb-base
                     "checkers-movement.rkt"))

(define-syntax (board stx)
  (syntax-parse stx
    [(_ (space ...) ...+)
     (define first-row-start (syntax-position (cadr (syntax->list stx))))
     (define first-row-end (+ first-row-start (syntax-span (cadr (syntax->list stx))) -2))
     (define last-row-start (syntax-position (last (syntax->list stx))))
     (define last-row-end (+ last-row-start (syntax-span (last (syntax->list stx))) -2))
     (attach-keybindings
      #'(list (list 'space ...)
              ...)
      (list (make-kb "c:s:l;c:s:u"
                     (move-left-up-diagonal first-row-start
                                            first-row-end
                                            last-row-start
                                            last-row-end)
                     "move-left-diagonal"
                     'local
                     stx)
            (make-kb "c:s:r;c:s:u"
                     (move-right-up-diagonal first-row-start
                                             first-row-end
                                             last-row-start
                                             last-row-end)
                     "move-right-diagonal"
                     'local
                     stx)
            (make-kb "c:s:r;c:s:d"
                     (move-right-down-diagonal first-row-start
                                               first-row-end
                                               last-row-start
                                               last-row-end)
                     "move-right-down-diagonal"
                     'local
                     stx)
            (make-kb "c:s:l;c:s:d"
                     (move-left-down-diagonal first-row-start
                                              first-row-end
                                              last-row-start
                                              last-row-end)
                     "move-left-down-diagonal"
                     'local
                     stx)))]))

;; A fresh board
#;(board (_ b _ b _ b _ b)
         (b _ b _ b _ b _)
         (_ b _ b _ b _ b)
         (_ _ _ _ _ _ _ _)
         (_ _ _ _ _ _ _ _)
         (r _ r _ r _ r _)
         (_ r _ r _ r _ r)
         (r _ r _ r _ r _))

(define initial-editor-content
  (string-append "(board (_ b _ b _ b _ b)\n"
                 "       (b _ b _ b _ b _)\n"
                 "       (_ b _ b _ b _ b)\n"
                 "       (_ _ _ _ _ _ _ _)\n"
                 "       (_ _ _ _ _ _ _ _)\n"
                 "       (r _ r _ r _ r _)\n"
                 "       (_ r _ r _ r _ r)\n"
                 "       (r _ r _ r _ r _))"))

(define left-up (move-left-up-diagonal 8 23 183 198))
(define right-up (move-right-up-diagonal 8 23 183 198))
(define left-down (move-left-down-diagonal 8 23 183 198))
(define right-down (move-right-down-diagonal 8 23 183 198))

;; moving up to the left as red
(test-kb-program
 left-up
 (λ (result-val result-pos result-text)
   (check-equal? result-text (string-append "(board (_ b _ b _ b _ b)\n"
                                            "       (b _ b _ b _ b _)\n"
                                            "       (_ b _ b _ b _ b)\n"
                                            "       (_ _ _ _ _ _ _ _)\n"
                                            "       (_ _ _ _ _ r _ _)\n"
                                            "       (r _ r _ r _ _ _)\n"
                                            "       (_ r _ r _ r _ r)\n"
                                            "       (r _ r _ r _ r _))"))) 
 #:setup-proc (λ (editor)
                (send editor insert initial-editor-content)
                (send editor set-position 145)))

;; moving up to the right as red
(test-kb-program
 right-up
 (λ (result-val result-pos result-text)
   (check-equal? result-text (string-append "(board (_ b _ b _ b _ b)\n"
                                            "       (b _ b _ b _ b _)\n"
                                            "       (_ b _ b _ b _ b)\n"
                                            "       (_ _ _ _ _ _ _ _)\n"
                                            "       (_ _ _ _ _ _ _ r)\n"
                                            "       (r _ r _ r _ _ _)\n"
                                            "       (_ r _ r _ r _ r)\n"
                                            "       (r _ r _ r _ r _))"))) 
 #:setup-proc (λ (editor)
                (send editor insert initial-editor-content)
                (send editor set-position 145)))

;; moving down to the left as black
(test-kb-program
 left-down
 (λ (result-val result-pos result-text)
   (check-equal? result-text (string-append "(board (_ b _ b _ b _ b)\n"
                                            "       (b _ b _ b _ b _)\n"
                                            "       (_ b _ _ _ b _ b)\n"
                                            "       (_ _ b _ _ _ _ _)\n"
                                            "       (_ _ _ _ _ _ _ _)\n"
                                            "       (r _ r _ r _ r _)\n"
                                            "       (_ r _ r _ r _ r)\n"
                                            "       (r _ r _ r _ r _))"))) 
 #:setup-proc (λ (editor)
                (send editor insert initial-editor-content)
                (send editor set-position 64)))

;; moving down to the right as black
(test-kb-program
 right-down
 (λ (result-val result-pos result-text)
   (check-equal? result-text (string-append "(board (_ b _ b _ b _ b)\n"
                                            "       (b _ b _ b _ b _)\n"
                                            "       (_ b _ _ _ b _ b)\n"
                                            "       (_ _ _ _ b _ _ _)\n"
                                            "       (_ _ _ _ _ _ _ _)\n"
                                            "       (r _ r _ r _ r _)\n"
                                            "       (_ r _ r _ r _ r)\n"
                                            "       (r _ r _ r _ r _))"))) 
 #:setup-proc (λ (editor)
                (send editor insert initial-editor-content)
                (send editor set-position 64)))