#lang racket
(require "GameState.rkt")         ; provides all of the functions required to manipulate the game-state
(require "UI.rkt")                ; provides "show-dialogue" function for displaying messages
(require "ItemsAndMinigames.rkt") ; provides all of the mini game functions

(provide menu-list)

; --- TITLE MENU -------------------------------------------------------------------------------

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


; --- MAIN MENU -------------------------------------------------------------------------------

(define main-menu
  (list "main-menu"
        [list "Unlock north path (5 coins)" "A" (lambda (game-state)
                                                   {begin
                                                     (cond
                                                       [(< (get-coin-count game-state) 5)
                                                        (show-dialogue (format "As you look pointlessly at the blocked passage, suddenly, an inexplicable feeling crosses your mind that\nyou need 5 coins to get through; current coins: ~a" (get-coin-count game-state)))
                                                        (game-loop game-state)]
                                                                            
                                                       [else
                                                        (show-dialogue "You unlocked the north path")
                                                        (let* ([state-1 (remove-menu-item "main-menu" "A" game-state)]
                                                               [state-2 (add-menu-item state-1 "main-menu" "Go north" "A" (lambda (game-state)
                                                                                                                           {begin
                                                                                                                             [show-dialogue "You take the north path"]
                                                                                                                             [define state-1 (set-current-menu "north-path" game-state)]
                                                                                                                             [game-loop state-1]}))]
                                                               [state-3 (deduct-coins 5 state-2)])
                                                          (game-loop state-3))]
                                                       )}
                                                   )]                             
        [list "Unlock south path (250 coins)" "B" (lambda (game-state)
                                                    {begin
                                                      (cond
                                                        [(< (get-coin-count game-state) 250)
                                                         (show-dialogue (format "As you look pointlessly at the blocked passage, suddenly, an inexplicable feeling crosses your mind that\nyou need 250 coins to get through; current coins: ~a" (get-coin-count game-state)))
                                                         (game-loop game-state)]

                                                        [else
                                                         (show-dialogue "You unlocked the south path")
                                                         (let* ([state-1 (remove-menu-item "main-menu" "B" game-state)]
                                                                [state-2 (add-menu-item state-1 "main-menu" "Go south" "B" (lambda (game-state)
                                                                                                                             {begin
                                                                                                                               [show-dialogue "You take the south path"]
                                                                                                                               [define state-1 (set-current-menu "south-path" game-state)]
                                                                                                                               [game-loop state-1]}))]
                                                                [state-3 (deduct-coins 250 state-2)])
                                                           (game-loop state-3))]
                                                        )}
                                                    )]
        [list "Unlock west path (10,000 coins)" "C" (lambda (game-state)
                                                      {begin
                                                        (cond
                                                          [(< (get-coin-count game-state) 10000)
                                                           (show-dialogue (format "As you look pointlessly at the blocked passage, suddenly, an inexplicable feeling crosses your mind that you need 10,000 coins to get through; current coins: ~a" (get-coin-count game-state)))
                                                           (game-loop game-state)]

                                                          [else
                                                           (show-dialogue "You unlocked the west path")
                                                           (let* ([state-1 (remove-menu-item "main-menu" "C" game-state)]
                                                                  [state-2 (add-menu-item state-1 "main-menu" "Go west" "C" (lambda (game-state)
                                                                                                                               {begin
                                                                                                                                 [show-dialogue "You take the west path"]
                                                                                                                                 [define state-1 (set-current-menu "west-path" game-state)]
                                                                                                                                 [game-loop state-1]}))]
                                                                  [state-3 (deduct-coins 10000 state-2)])
                                                             (game-loop state-3))]
                                                          )}
                                                      )]
        [list "Unlock east path (150,000 coins)" "D" (lambda (game-state)
                                                       {begin
                                                        (cond
                                                          [(< (get-coin-count game-state) 150000)
                                                           (show-dialogue (format "As you look pointlessly at the blocked passage, suddenly, an inexplicable feeling crosses your mind that you need 150,000 coins to get through; current coins: ~a" (get-coin-count game-state)))
                                                           (game-loop game-state)]

                                                          [else
                                                           (show-dialogue "You unlocked the east path")
                                                           (let* ([state-1 (remove-menu-item "main-menu" "D" game-state)]
                                                                  [state-2 (add-menu-item state-1 "main-menu" "Go east" "D" (lambda (game-state)
                                                                                                                               {begin
                                                                                                                                 [show-dialogue "You take the east path"]
                                                                                                                                 [define state-1 (set-current-menu "east-path" game-state)]
                                                                                                                                 [game-loop state-1]}))]
                                                                  [state-3 (deduct-coins 150000 state-2)])
                                                             (game-loop state-3))]
                                                          )}
                                                      )]
        [list "Escape (1,000,000 coins)" "E" (lambda (game-state) "todo")]
        [list "Check your surroundings" "S" (lambda (game-state)
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
                                                                                                                                  [define state-5 (add-menu-item state-4 "main-menu" "Upgrade coin generator" "U" (lambda (game-state)
                                                                                                                                                                                                                    {begin
                                                                                                                                                                                                                      [define state-1 (set-current-menu "upgrade-menu" game-state)]
                                                                                                                                                                                                                      [game-loop state-1]
                                                                                                                                                                                                                      }))]
                                                                                                                                  [game-loop state-5]
                                                                                                                                }))]
                                                [define state-2 (remove-menu-item "main-menu" "S" state-1)]
                                                [game-loop state-2]
                                                 })]
        [list "Info" "Z" (lambda (game-state)
                                             {begin
                                               [display-inventory game-state]
                                               [displayln "  ---"]
                                               [displayln (format "   COINS: ~a" (get-coin-count game-state))]
                                               [displayln "  ---"]
                                               [displayln "   CURRENT LOCATION: MAIN AREA"]
                                               [displayln "  ---"]
                                               [displayln "   BRIEF DESCRIPTION: a desolate cave with 4 branching paths"]
                                               [show-dialogue ""]
                                               [game-loop game-state]
                                               })]
        ))

; --- NORTH PATH MENU -------------------------------------------------------------------------------
(define north-path
  (list "north-path"
        [list "Check your surroundings" "S" (lambda (game-state)
                                              {begin
                                                [show-dialogue "The chamber this passage has led you to is small and cramped.  To your left you are surpised to see a vending machine. Maybe you can get some food before you starve.\nTo your right there is a locked passage"]
                                                [define state-1 (add-menu-item game-state "north-path" "Use vending machine (7 coins)" "A" (lambda (game-state) "todo"))]
                                                [define state-2 (add-menu-item state-1 "north-path" "Unlock passage (1 key)" "B" (lambda (game-state)
                                                                                                                                   {begin
                                                                                                                                     (cond
                                                                                                                                       [(item-in-inventory? "KEY " game-state)
                                                                                                                                        (let* ([state-1 (use-item "KEY " game-state)]
                                                                                                                                               [state-2 (remove-menu-item "north-path" "B" state-1)]
                                                                                                                                               [state-3 (add-menu-item state-2 "north-path" "Enter passage" "B" (lambda (game-state) "todo"))])
                                                                                                                                          
                                                                                                                                          [show-dialogue "You use the key to unlock the passage"]
                                                                                                                                          [game-loop state-3]
                                                                                                                                          )]
                                                                                                                                       
                                                                                                                                       [else
                                                                                                                                        [show-dialogue "You stare helplessly at the locked passage hoping that by some miracle it will open"]
                                                                                                                                        [show-dialogue "After staring for what seems like an eternity, you give up"]
                                                                                                                                        [game-loop game-state]]
                                                                                                                                       )}
                                                                                                                                   ))]
                                                [define state-3 (remove-menu-item "north-path" "S" state-2)]
                                                [game-loop state-3]
                                                })]
        [list "Go back to main area" "R" (lambda (game-state)
                                           {begin
                                             [show-dialogue "You head back to the main area"]
                                             [define state-1 (set-current-menu "main-menu" game-state)]
                                             [game-loop state-1]
                                             })]
        [list "Info" "Z" (lambda (game-state)
                                             {begin
                                               [display-inventory game-state]
                                               [displayln "  ---"]
                                               [displayln (format "   COINS: ~a" (get-coin-count game-state))]
                                               [displayln "  ---"]
                                               [displayln "   CURRENT LOCATION: NORTH PATH"]
                                               [displayln "  ---"]
                                               [displayln "   BRIEF DESCRIPTION: a small area with a vending machine and a passage that may or may not be locked"]
                                               [show-dialogue ""]
                                               [game-loop game-state]
                                               })]
        ))

(define menu-list (list title main-menu north-path))