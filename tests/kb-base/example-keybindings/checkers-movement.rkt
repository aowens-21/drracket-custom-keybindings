#lang racket/base

(require syntax/parse
         (for-syntax kb-base
                     racket/base
                     syntax/parse
                     racket/set))

(define-for-syntax (move-checker-direction direction-pair current-color)
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
                      (insert 'original-piece)
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
                                     (insert 'original-piece)
                                     (set-position 'dest-space)
                                     (delete 1)
                                     (insert "_")
                                     (set-position 'original-space)
                                     (delete 1)
                                     (insert "_"))
                                ;; jump dest is occupied, do nothing
                                (seq (get-character))))
                        ;; dest is same color, do nothing
                        (seq (get-character)))))
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
                                            (insert "_"))
                                       ;; jump dest is occupied, do nothing
                                       (seq (get-character))))
                               ;; dest is same color, do nothing
                               (seq (get-character))))))))

(define-for-syntax move-left-up-diagonal
  (move-checker-direction (cons 'left 'up) #\r))
  #;(kb-if (kb-or (kb-equal? (get-character)
                           #\r)
                (kb-equal? (get-character)
                           #\R))
         (kb-let
          (['original-space (get-position)]
           ['original-piece (get-text 'original-space
                                      (add 'original-space
                                              1))]
           ['dest-space (seq (move-position 'up 'simple)
                             (do-times 2 (move-position 'left 'simple))
                             (get-position))])
          (kb-if (kb-equal? (get-character)
                            #\_)
                 ;; dest is empty, just do the move
                 (seq (delete 1)
                      (insert 'original-piece)
                      (set-position 'original-space)
                      (delete 1)
                      (insert "_"))
                 (kb-if (kb-or (kb-equal? (get-character)
                                          #\b)
                               (kb-equal? (get-character)
                                          #\B))
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
                                     (insert 'original-piece)
                                     (set-position 'dest-space)
                                     (delete 1)
                                     (insert "_")
                                     (set-position 'original-space)
                                     (delete 1)
                                     (insert "_"))
                                ;; jump dest is occupied, do nothing
                                (seq (get-character))))
                        ;; dest is same color, do nothing
                        (seq (get-character)))))
         (kb-if (kb-equal? (get-character)
                           #\B)
                (kb-let
                 (['original-space (get-position)]
                  ['dest-space (seq (move-position 'up 'simple)
                                    (do-times 2 (move-position 'left 'simple))
                                    (get-position))])
                 (kb-if (kb-equal? (get-character)
                                   #\_)
                        ;; dest is empty, just do the move
                        (seq (delete 1)
                             (insert "B")
                             (set-position 'original-space)
                             (delete 1)
                             (insert "_"))
                        (kb-if (kb-or (kb-equal? (get-character)
                                                 #\r)
                                      (kb-equal? (get-character)
                                                 #\R))
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
                                            (insert "B")
                                            (set-position 'dest-space)
                                            (delete 1)
                                            (insert "_")
                                            (set-position 'original-space)
                                            (delete 1)
                                            (insert "_"))
                                       ;; jump dest is occupied, do nothing
                                       (seq (get-character))))
                               ;; dest is same color, do nothing
                               (seq (get-character)))))))

(define-for-syntax move-right-up-diagonal
  (kb-if (kb-or (kb-equal? (get-character)
                           #\r)
                (kb-equal? (get-character)
                           #\R))
         (kb-let
          (['original-space (get-position)]
           ['original-piece (get-text 'original-space
                                      (add 'original-space
                                              1))]
           ['dest-space (seq (move-position 'up 'simple)
                             (do-times 2 (move-position 'right 'simple))
                             (get-position))])
          (kb-if (kb-equal? (get-character)
                            #\_)
                 ;; dest is empty, just do the move
                 (seq (delete 1)
                      (insert 'original-piece)
                      (set-position 'original-space)
                      (delete 1)
                      (insert "_"))
                 (kb-if (kb-or (kb-equal? (get-character)
                                          #\b)
                               (kb-equal? (get-character)
                                          #\B))
                        ;; dest is opposite color, check if we can jump
                        (kb-let
                         (['jump-dest (seq (move-position 'up 'simple)
                                           (do-times 2 (move-position 'right 'simple))
                                           (get-position))])
                         (kb-if (kb-equal? (get-character)
                                           #\_)
                                ;; jump dest is free, do the jump
                                ;; deleting jumped checker and original pos
                                (seq (delete 1)
                                     (insert 'original-piece)
                                     (set-position 'dest-space)
                                     (delete 1)
                                     (insert "_")
                                     (set-position 'original-space)
                                     (delete 1)
                                     (insert "_"))
                                ;; jump dest is occupied, do nothing
                                (seq (get-character))))
                        ;; dest is same color, do nothing
                        (seq (get-character)))))
         (kb-if (kb-equal? (get-character)
                           #\B)
                (kb-let
                 (['original-space (get-position)]
                  ['dest-space (seq (move-position 'up 'simple)
                                    (do-times 2 (move-position 'right 'simple))
                                    (get-position))])
                 (kb-if (kb-equal? (get-character)
                                   #\_)
                        ;; dest is empty, just do the move
                        (seq (delete 1)
                             (insert "B")
                             (set-position 'original-space)
                             (delete 1)
                             (insert "_"))
                        (kb-if (kb-or (kb-equal? (get-character)
                                                 #\r)
                                      (kb-equal? (get-character)
                                                 #\R))
                               ;; dest is opposite color, check if we can jump
                               (kb-let
                                (['jump-dest (seq (move-position 'up 'simple)
                                                  (do-times 2 (move-position 'right 'simple))
                                                  (get-position))])
                                (kb-if (kb-equal? (get-character)
                                                  #\_)
                                       ;; jump dest is free, do the jump
                                       ;; deleting jumped checker and original pos
                                       (seq (delete 1)
                                            (insert "B")
                                            (set-position 'dest-space)
                                            (delete 1)
                                            (insert "_")
                                            (set-position 'original-space)
                                            (delete 1)
                                            (insert "_"))
                                       ;; jump dest is occupied, do nothing
                                       (seq (get-character))))
                               ;; dest is same color, do nothing
                               (seq (get-character))))))))

(define-for-syntax move-left-down-diagonal
  (kb-if (kb-or (kb-equal? (get-character)
                           #\b)
                (kb-equal? (get-character)
                           #\B))
         (kb-let
          (['original-space (get-position)]
           ['original-piece (get-text 'original-space
                                      (add 'original-space
                                              1))]
           ['dest-space (seq (move-position 'down 'simple)
                             (do-times 2 (move-position 'left 'simple))
                             (get-position))])
          (kb-if (kb-equal? (get-character)
                            #\_)
                 ;; dest is empty, just do the move
                 (seq (delete 1)
                      (insert 'original-piece)
                      (set-position 'original-space)
                      (delete 1)
                      (insert "_"))
                 (kb-if (kb-or (kb-equal? (get-character)
                                          #\r)
                               (kb-equal? (get-character)
                                          #\R))
                        ;; dest is opposite color, check if we can jump
                        (kb-let
                         (['jump-dest (seq (move-position 'down 'simple)
                                           (do-times 2 (move-position 'left 'simple))
                                           (get-position))])
                         (kb-if (kb-equal? (get-character)
                                           #\_)
                                ;; jump dest is free, do the jump
                                ;; deleting jumped checker and original pos
                                (seq (delete 1)
                                     (insert 'original-piece)
                                     (set-position 'dest-space)
                                     (delete 1)
                                     (insert "_")
                                     (set-position 'original-space)
                                     (delete 1)
                                     (insert "_"))
                                ;; jump dest is occupied, do nothing
                                (seq (get-character))))
                        ;; dest is same color, do nothing
                        (seq (get-character)))))
         (kb-if (kb-equal? (get-character)
                           #\R)
                (kb-let
                 (['original-space (get-position)]
                  ['dest-space (seq (move-position 'down 'simple)
                                    (do-times 2 (move-position 'left 'simple))
                                    (get-position))])
                 (kb-if (kb-equal? (get-character)
                                   #\_)
                        ;; dest is empty, just do the move
                        (seq (delete 1)
                             (insert "R")
                             (set-position 'original-space)
                             (delete 1)
                             (insert "_"))
                        (kb-if (kb-or (kb-equal? (get-character)
                                                 #\b)
                                      (kb-equal? (get-character)
                                                 #\B))
                               ;; dest is opposite color, check if we can jump
                               (kb-let
                                (['jump-dest (seq (move-position 'down 'simple)
                                                  (do-times 2 (move-position 'left 'simple))
                                                  (get-position))])
                                (kb-if (kb-equal? (get-character)
                                                  #\_)
                                       ;; jump dest is free, do the jump
                                       ;; deleting jumped checker and original pos
                                       (seq (delete 1)
                                            (insert "R")
                                            (set-position 'dest-space)
                                            (delete 1)
                                            (insert "_")
                                            (set-position 'original-space)
                                            (delete 1)
                                            (insert "_"))
                                       ;; jump dest is occupied, do nothing
                                       (seq (get-character))))
                               ;; dest is same color, do nothing
                               (seq (get-character))))))))

(define-for-syntax move-right-down-diagonal
  (kb-if (kb-or (kb-equal? (get-character)
                           #\b)
                (kb-equal? (get-character)
                           #\B))
         (kb-let
          (['original-space (get-position)]
           ['original-piece (get-text 'original-space
                                      (add 'original-space
                                              1))]
           ['dest-space (seq (move-position 'down 'simple)
                             (do-times 2 (move-position 'right 'simple))
                             (get-position))])
          (kb-if (kb-equal? (get-character)
                            #\_)
                 ;; dest is empty, just do the move
                 (seq (delete 1)
                      (insert 'original-piece)
                      (set-position 'original-space)
                      (delete 1)
                      (insert "_"))
                 (kb-if (kb-or (kb-equal? (get-character)
                                          #\r)
                               (kb-equal? (get-character)
                                          #\R))
                        ;; dest is opposite color, check if we can jump
                        (kb-let
                         (['jump-dest (seq (move-position 'down 'simple)
                                           (do-times 2 (move-position 'right 'simple))
                                           (get-position))])
                         (kb-if (kb-equal? (get-character)
                                           #\_)
                                ;; jump dest is free, do the jump
                                ;; deleting jumped checker and original pos
                                (seq (delete 1)
                                     (insert 'original-piece)
                                     (set-position 'dest-space)
                                     (delete 1)
                                     (insert "_")
                                     (set-position 'original-space)
                                     (delete 1)
                                     (insert "_"))
                                ;; jump dest is occupied, do nothing
                                (seq (get-character))))
                        ;; dest is same color, do nothing
                        (seq (get-character)))))
         (kb-if (kb-equal? (get-character)
                           #\R)
                (kb-let
                 (['original-space (get-position)]
                  ['dest-space (seq (move-position 'down 'simple)
                                    (do-times 2 (move-position 'right 'simple))
                                    (get-position))])
                 (kb-if (kb-equal? (get-character)
                                   #\_)
                        ;; dest is empty, just do the move
                        (seq (delete 1)
                             (insert "R")
                             (set-position 'original-space)
                             (delete 1)
                             (insert "_"))
                        (kb-if (kb-or (kb-equal? (get-character)
                                                 #\b)
                                      (kb-equal? (get-character)
                                                 #\B))
                               ;; dest is opposite color, check if we can jump
                               (kb-let
                                (['jump-dest (seq (move-position 'down 'simple)
                                                  (do-times 2 (move-position 'right 'simple))
                                                  (get-position))])
                                (kb-if (kb-equal? (get-character)
                                                  #\_)
                                       ;; jump dest is free, do the jump
                                       ;; deleting jumped checker and original pos
                                       (seq (delete 1)
                                            (insert "R")
                                            (set-position 'dest-space)
                                            (delete 1)
                                            (insert "_")
                                            (set-position 'original-space)
                                            (delete 1)
                                            (insert "_"))
                                       ;; jump dest is occupied, do nothing
                                       (seq (get-character))))
                               ;; dest is same color, do nothing
                               (seq (get-character))))))))

(define-syntax (board stx)
  (syntax-parse stx
    [(_ (space ...) ...+)
     (attach-keybindings
      #'(list (list 'space ...)
              ...)
      (list (make-kb "c:s:l;c:s:u"
                     move-left-up-diagonal
                     "move-left-diagonal"
                     'local
                     stx)
            (make-kb "c:s:r;c:s:u"
                     move-right-up-diagonal
                     "move-right-diagonal"
                     'local
                     stx)
            (make-kb "c:s:r;c:s:d"
                     move-right-down-diagonal
                     "move-right-down-diagonal"
                     'local
                     stx)
            (make-kb "c:s:l;c:s:d"
                     move-left-down-diagonal
                     "move-left-down-diagonal"
                     'local
                     stx)))]))

;; A fresh board
(board (_ b _ b _ b _ b)
       (b _ b _ b _ b _)
       (_ _ _ b _ b _ b)
       (_ _ r _ r _ _ _)
       (_ _ _ _ _ _ _ _)
       (r _ r _ _ _ _ _)
       (_ r _ r _ r _ r)
       (r _ r _ r _ r _))