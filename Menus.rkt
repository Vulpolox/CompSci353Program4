#lang racket
(require "GameState.rkt")
(require "UI.rkt")
(require "ItemsAndMinigames.rkt")

(provide menu-list)


(define title 
  (list "title"
        [list "Play Game" "A" (lambda (game-state)
                                {begin
                                 (show-dialogue "You awaken and find yourself in an expansive cave.  How will you escape?")
                                 (define new-state (set-current-menu "main-menu" game-state))
                                 (game-loop new-state)
                                }
                                )]
        [list "Exit Game" "B" (lambda (game-state) {display "THANKS FOR PLAYING"})]
        ))

(define main-menu
  (list "main-menu"
        [list "Unlock north path (10 coins)" "A" (lambda (game-state)
                                                   {begin
                                                     (cond
                                                       [(< (get-coin-count game-state) 10)
                                                        (show-dialogue (format "You don't have enough coins; current coins: ~a" (get-coin-count game-state)))
                                                        (game-loop game-state)]
                                                                            
                                                       [else
                                                        (show-dialogue "You unlocked the north path")
                                                        (let* ([state-1 (remove-menu-item "main-menu" "A" game-state)]
                                                               [state-2 (add-menu-item state-1 "main-menu" "Go north" "A" (lambda (game-state)
                                                                                                                           {begin
                                                                                                                             [define state-1 (set-current-menu "north-path" game-state)]
                                                                                                                             [game-loop state-1]}))])
                                                          (game-loop state-2))]
                                                       )}
                                                   )]                             
        [list "Unlock south path (250 coins)" "B" (lambda (game-state)
                                                    {begin
                                                      (cond
                                                        [(< (get-coin-count game-state) 250)
                                                         (show-dialogue (format "You don't have enough coins; current coins: ~a" (get-coin-count game-state)))
                                                         (game-loop game-state)]

                                                        [else
                                                         (show-dialogue "You unlocked the south path")
                                                         (let* ([state-1 (remove-menu-item "main-menu" "B" game-state)]
                                                                [state-2 (add-menu-item state-1 "main-menu" "Go south" "B" (lambda (game-state) "todo"))])
                                                           "todo"
                                                           )])})]
        [list "Unlock west path (10,000 coins)" "C" (lambda (game-state) "todo")]
        [list "Unlock east path (150,000 coins)" "D" (lambda (game-state) "todo")]
        [list "Escape (1,000,000 coins)" "E" (lambda (game-state) "todo")]
        [list "Check your surroundings" "G" (lambda (game-state)
                                              {begin
                                                [show-dialogue "You take a look around. There are four blocked paths in each of the cardinal directions but no way to advance.\nOut of the corner of your eye, you spot a strange device"]
                                                [define state-1 (add-menu-item game-state "main-menu" "Pick up the strange device" "P" (lambda (game-state)
                                                                                                                                {begin
                                                                                                                                  [show-dialogue "Upon closer examination of the device, you find a button. Upon pressing it, a golden coin drops to the ground"]
                                                                                                                                  [define state-1 (pick-up-item "COIN GENERATOR "
                                                                                                                                                                  game-state)]
                                                                                                                                  [define state-2 (remove-menu-item "main-menu" "P" state-1)]
                                                                                                                                  [define state-3 (increment-click-amount 1 state-2)]
                                                                                                                                  [define state-4 (add-menu-item state-3 "main-menu" "Press the coin button" "P" (lambda (game-state)
                                                                                                                                                                                                              {begin
                                                                                                                                                                                                                [define state-1 (click game-state)]
                                                                                                                                                                                                                [game-loop state-1]
                                                                                                                                                                                                                }))]
                                                                                                                                  [game-loop state-4]
                                                                                                                                }))]
                                                [define state-2 (remove-menu-item "main-menu" "G" state-1)]
                                                [game-loop state-2]
                                                 })]
        ))

(define menu-list (list title main-menu))