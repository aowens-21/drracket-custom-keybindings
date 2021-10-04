#lang racket/base

(require kb-base)

(provide move-left-up-diagonal
         move-right-up-diagonal
         move-left-down-diagonal
         move-right-down-diagonal)

(define (move-checker-direction direction-pair current-color king-row-pair)
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
                                    (insert "B"))
                             (insert 'original-piece))
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
                                                   (insert "B"))
                                            (insert 'original-piece))
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

(define (move-left-up-diagonal
                    first-start
                    first-end
                    last-start
                    last-end)
  (move-checker-direction (cons 'left 'up)
                          #\r
                          (cons first-start first-end)))
  
(define (move-right-up-diagonal
                    first-start
                    first-end
                    last-start
                    last-end)
  (move-checker-direction (cons 'right 'up)
                          #\r
                          (cons first-start first-end)))

(define (move-left-down-diagonal
                    first-start
                    first-end
                    last-start
                    last-end)
  (move-checker-direction (cons 'left 'down)
                          #\b
                          (cons last-start last-end)))

(define (move-right-down-diagonal
                    first-start
                    first-end
                    last-start
                    last-end)
  (move-checker-direction (cons 'right 'down)
                          #\b
                          (cons last-start last-end)))