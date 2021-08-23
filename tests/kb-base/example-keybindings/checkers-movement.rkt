#lang racket/base

(require syntax/parse
         (for-syntax kb-base
                     racket/base
                     syntax/parse))

(define-for-syntax move-left-diagonal
  (kb-if (kb-equal? (get-character)
                    #\r)
         ;; moving up
         (kb-let
          (['original-space (get-position)]
           ['dest-space (seq (move-position 'up 'simple)
                             (do-times 2 (move-position 'left 'simple))
                             (get-position))])
          (kb-if (kb-equal? (get-character)
                            #\_)
                 ;; dest is empty, just do the move
                 (seq (delete 1)
                      (insert "r")
                      (set-position 'original-space)
                      (delete 1)
                      (insert "_"))
                 (kb-if (kb-equal? (get-character)
                                   #\b)
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
                                     (insert "r")
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
                 
          ;; moving down
          (kb-if (kb-equal? (get-character)
                            #\b)
                 (kb-let
                  (['original-space (get-position)]
                   ['dest-space (seq (move-position 'down 'simple)
                                     (do-times 2 (move-position 'left 'simple))
                                     (get-position))])
                  (kb-if (kb-equal? (get-character)
                                    #\_)
                         ;; dest is empty, just do the move
                         (seq (delete 1)
                              (insert "b")
                              (set-position 'original-space)
                              (delete 1)
                              (insert "_"))
                         (kb-if (kb-equal? (get-character)
                                           #\r)
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
                                             (insert "b")
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
                  (seq (get-character)))))

(define-for-syntax move-right-diagonal
  (kb-if (kb-equal? (get-character)
                    #\r)
         ;; moving up
         (kb-let
          (['original-space (get-position)]
           ['dest-space (seq (move-position 'up 'simple)
                             (do-times 2 (move-position 'right 'simple))
                             (get-position))])
          (kb-if (kb-equal? (get-character)
                            #\_)
                 ;; dest is empty, just do the move
                 (seq (delete 1)
                      (insert "r")
                      (set-position 'original-space)
                      (delete 1)
                      (insert "_"))
                 (kb-if (kb-equal? (get-character)
                                   #\b)
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
                                     (insert "r")
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
                 
          ;; moving down
          (kb-if (kb-equal? (get-character)
                            #\b)
                 (kb-let
                  (['original-space (get-position)]
                   ['dest-space (seq (move-position 'down 'simple)
                                     (do-times 2 (move-position 'right 'simple))
                                     (get-position))])
                  (kb-if (kb-equal? (get-character)
                                    #\_)
                         ;; dest is empty, just do the move
                         (seq (delete 1)
                              (insert "b")
                              (set-position 'original-space)
                              (delete 1)
                              (insert "_"))
                         (kb-if (kb-equal? (get-character)
                                           #\r)
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
                                             (insert "b")
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
                  (seq (get-character)))))

(define-syntax (board stx)
  (syntax-parse stx
    [(_ (space ...) ...+)
     (syntax-property
      #'(list (list 'space ...)
              ...)
      'keybinding-info
      (make-kb "c:space"
               move-left-diagonal
               "move-left-diagonal"
               'local
               stx))]))

(board (_ _ _ _ _ _)
       (_ _ _ _ _ _)
       (_ _ _ b _ _)
       (_ r _ b _ _))