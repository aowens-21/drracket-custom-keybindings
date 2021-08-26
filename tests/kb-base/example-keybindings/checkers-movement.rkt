#lang racket/base

(require syntax/parse
         racket/list
         (for-syntax kb-base
                     racket/list
                     racket/base
                     syntax/parse
                     racket/set))

(define-for-syntax (move-checker-direction direction-pair current-color king-row-pair)
  (define kinged-current-color (if (char=? current-color #\r)
                                     #\R
                                     #\B))
  (define opponent-color (if (eq? current-color #\r)
                             #\b
                             #\r))
  (define kinged-opponent-color (if (eq? current-color #\r)
                                   #\B
                                   #\R))
  (define dx (car direction-pair))
  (define dy (cdr direction-pair))
  (define king-row-start (car king-row-pair))
  (define king-row-end (cdr king-row-pair))
  
  (kb-if (kb-or (kb-equal? (get-character)
                           current-color)
                (kb-equal? (get-character)
                           kinged-current-color))
         (kb-let
          (['original-space (get-position)]
           ['original-piece (get-text 'original-space
                                      (add 'original-space
                                              1))]
           ['dest-space (seq (move-position dy 'simple)
                             (do-times 2 (move-position dx 'simple))
                             (get-position))])
          (kb-if (kb-equal? (get-character)
                            #\_)
                 ;; dest is empty, just do the move
                 (seq (delete 1)
                      (kb-if (kb-and (kb-gte (get-position)
                                             king-row-start)
                                     (kb-lte (get-position)
                                             king-row-end))
                             (kb-if (kb-equal? 'original-piece "r")
                                    (insert "R")
                                    (insert "B")))
                      (set-position 'original-space)
                      (delete 1)
                      (insert "_"))
                 (kb-if (kb-or (kb-equal? (get-character)
                                          opponent-color)
                               (kb-equal? (get-character)
                                          kinged-opponent-color))
                        ;; dest is opposite color, check if we can jump
                        (kb-let
                         (['jump-dest (seq (move-position dy 'simple)
                                           (do-times 2 (move-position dx 'simple))
                                           (get-position))])
                         (kb-if (kb-equal? (get-character)
                                           #\_)
                                ;; jump dest is free, do the jump
                                ;; deleting jumped checker and original pos
                                (seq (delete 1)
                                     (kb-if (kb-and (kb-gte (get-position)
                                                            king-row-start)
                                                    (kb-lte (get-position)
                                                            king-row-end))
                                            (kb-if (kb-equal? 'original-piece "r")
                                                   (insert "R")
                                                   (insert "B")))
                                     (set-position 'dest-space)
                                     (delete 1)
                                     (insert "_")
                                     (set-position 'original-space)
                                     (delete 1)
                                     (insert "_")))))))
         (kb-if (kb-equal? (get-character)
                           kinged-opponent-color)
                (kb-let
                 (['original-space (get-position)]
                  ['original-color (get-text 'original-space (add 'original-space 1))]
                  ['dest-space (seq (move-position dy 'simple)
                                    (do-times 2 (move-position dx 'simple))
                                    (get-position))])
                 (kb-if (kb-equal? (get-character)
                                   #\_)
                        ;; dest is empty, just do the move
                        (seq (delete 1)
                             (insert 'original-color)
                             (set-position 'original-space)
                             (delete 1)
                             (insert "_"))
                        (kb-if (kb-or (kb-equal? (get-character)
                                                 current-color)
                                      (kb-equal? (get-character)
                                                 kinged-current-color))
                               ;; dest is opposite color, check if we can jump
                               (kb-let
                                (['jump-dest (seq (move-position 'up 'simple)
                                                  (do-times 2 (move-position 'left 'simple))
                                                  (get-position))])
                                (kb-if (kb-equal? (get-character)
                                                  #\_)
                                       ;; jump dest is free, do the jump
                                       ;; deleting jumped checker and original pos
                                       (seq (delete 1)
                                            (insert 'original-color)
                                            (set-position 'dest-space)
                                            (delete 1)
                                            (insert "_")
                                            (set-position 'original-space)
                                            (delete 1)
                                            (insert "_"))))))))))

(define-for-syntax (move-left-up-diagonal
                    first-start
                    first-end
                    last-start
                    last-end)
  (move-checker-direction (cons 'left 'up)
                          #\r
                          (cons first-start first-end)))
  
(define-for-syntax (move-right-up-diagonal
                    first-start
                    first-end
                    last-start
                    last-end)
  (move-checker-direction (cons 'right 'up)
                          #\r
                          (cons first-start first-end)))

(define-for-syntax (move-left-down-diagonal
                    first-start
                    first-end
                    last-start
                    last-end)
  (move-checker-direction (cons 'left 'down)
                          #\b
                          (cons last-start last-end)))

(define-for-syntax (move-right-down-diagonal
                    first-start
                    first-end
                    last-start
                    last-end)
  (move-checker-direction (cons 'right 'down)
                          #\b
                          (cons last-start last-end)))

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

(board (_ b R b _ b _ b)
       (b _ _ _ b _ b _)
       (_ _ _ b _ b _ b)
       (_ _ _ _ _ _ _ _)
       (_ _ _ r _ _ _ _)
       (r _ _ _ r _ r _)
       (_ r _ _ _ r _ r)
       (r _ r _ B _ r _))

;; A fresh board
#;(board (_ b _ b _ b _ b)
       (b _ b _ b _ b _)
       (_ b _ b _ b _ b)
       (_ _ _ _ _ _ _ _)
       (_ _ _ _ _ _ _ _)
       (r _ r _ r _ r _)
       (_ r _ r _ r _ r)
       (r _ r _ r _ r _))