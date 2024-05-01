#lang racket
(require "GameState.rkt")         ; provides all of the functions required to manipulate the game-state
(require "UI.rkt")                ; provides "show-dialogue" function for displaying messages
(require "Minigames.rkt")         ; provides all of the mini game functions

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
        [list "Exit Game" "B" (lambda (game-state) {show-dialogue "THANKS FOR PLAYING"})]
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
        [list "Escape (20,000,000)" "E" (lambda (game-state) {lambda-extractor game-state "logic for unlocking victory-path"})]
        [list "Drop item" "W" (lambda (game-state)
                                {begin
                                  [define state-1 (drop-item game-state)]
                                  [game-loop state-1]
                                  })]
        [list "Check your surroundings" "S" (lambda (game-state)
                                              {begin
                                                [show-dialogue "You take a look around. There are four blocked paths in each of the cardinal directions but no way to advance.\nOut of the corner of your eye, you spot a strange device"]
                                                [define state-1 (add-menu-item game-state "main-menu" "Investigate the strange device" "I" (lambda (game-state)
                                                                                                                                {begin
                                                                                                                                  [show-dialogue "Upon closer examination of the device, you find a button. Upon pressing it, a golden coin drops to the ground"]
                                                                                                                                  [define state-1 (remove-menu-item "main-menu" "I" game-state)]
                                                                                                                                  [define state-2 (add-coins 1 state-1)]
                                                                                                                                  [define state-3 (add-menu-item state-2 "main-menu" "Press the coin button" "P" (lambda (game-state)
                                                                                                                                                                                                              {begin
                                                                                                                                                                                                                [define state-1 (click game-state)]
                                                                                                                                                                                                                [game-loop state-1]
                                                                                                                                                                                                                }))]
                                                                                                                                  [define state-4 (add-menu-item state-3 "main-menu" "Upgrade coin generator" "U" (lambda (game-state)
                                                                                                                                                                                                                    {begin
                                                                                                                                                                                                                      [define state-1 (set-current-menu "upgrade-menu" game-state)]
                                                                                                                                                                                                                      [game-loop state-1]
                                                                                                                                                                                                                      }))]
                                                                                                                                  [game-loop state-4]
                                                                                                                                }))]
                                                [define state-2 (remove-menu-item "main-menu" "S" state-1)]
                                                [game-loop state-2]
                                                 })]
        [list "Info" "Z" (lambda (game-state) {lambda-extractor game-state "logic for showing info"
                                                                #:current-location "MAIN AREA"
                                                                #:description "a desolate cave with 4 branching paths"})]
        ))

; --- UPGRADE MENU ----------------------------------------------------------------------------------

(define upgrade-menu
  (list "upgrade-menu"
        [list "Exit upgrade menu" "X" (lambda (game-state) {game-loop (set-current-menu "main-menu" game-state)})]
        [list "Upgrade 1  (1x UPGRADE MODULE V1)" "A" (lambda (game-state) {begin
                                                                            (cond
                                                                              [(not (item-in-inventory? "UPGRADE MODULE V1 " game-state))
                                                                               [show-dialogue "You don't have this upgrade yet"]
                                                                               [game-loop game-state]]

                                                                              [else
                                                                               (let* ([state-1 (use-item "UPGRADE MODULE V1 " game-state)]
                                                                                      [state-2 (remove-menu-item "upgrade-menu" "A" state-1)]
                                                                                      [state-3 (increment-click-amount 10 state-2)])
                                                                                 [show-dialogue "You insert the upgrade module into the coin generator"]
                                                                                 [show-dialogue "A screen lights up and displays a message: \"COIN AMOUNT + 10\""]
                                                                                 [game-loop state-3])]
                                                                              )})]
        [list "Upgrade 2  (1x UPGRADE MODULE V2)" "B" (lambda (game-state) {begin
                                                                            (cond
                                                                              [(not (item-in-inventory? "UPGRADE MODULE V2 " game-state))
                                                                               [show-dialogue "You don't have this upgrade yet"]
                                                                               [game-loop game-state]]

                                                                              [else
                                                                               (let* ([state-1 (remove-menu-item "upgrade-menu" "B" game-state)]
                                                                                      [state-2 (increment-click-amount 100 state-1)]
                                                                                      [state-3 (use-item "UPGRADE MODULE V2 " state-2)])
                                                                                 (show-dialogue "You insert the upgrade module in the coin generator")
                                                                                 (show-dialogue "Just like before, a screen lights up, this time saying: \"COIN AMOUNT + 100\"")
                                                                                 (game-loop state-3))]
                                                                              )})]
        [list "Upgrade 3  (1x UPGRADE MODULE V3)" "C" (lambda (game-state) {begin
                                                                            (cond
                                                                              [(not (item-in-inventory? "UPGRADE MODULE V3 " game-state))
                                                                               [show-dialogue "You don't have this upgrade yet"]
                                                                               [game-loop game-state]]

                                                                              [else
                                                                               (let* ([state-1 (remove-menu-item "upgrade-menu" "C" game-state)]
                                                                                      [state-2 (increment-click-amount 10000 state-1)]
                                                                                      [state-3 (use-item "UPGRADE MODULE V3 " state-2)])
                                                                                 [show-dialogue "You insert the upgrade module in the coin generator"]
                                                                                 [show-dialogue "The screen reads \"COIN AMOUNT + 10,000\""]
                                                                                 [game-loop state-3])]
                                                                              )})]
        [list "Upgrade 4  (1x UPGRADE MODULE V4)" "D" (lambda (game-state) {begin
                                                                            (cond
                                                                              [(not (item-in-inventory? "UPGRADE MODULE V4 " game-state))
                                                                               [show-dialogue "You don't have this upgrade yet"]
                                                                               [game-loop game-state]]

                                                                              [else
                                                                               (let* ([state-1 (remove-menu-item "upgrade-menu" "D" game-state)]
                                                                                      [state-2 (increment-click-amount 1000000 state-1)]
                                                                                      [state-3 (use-item "UPGRADE MODULE V4 " state-2)])
                                                                                 [show-dialogue "You insert the upgrade module into the coin generator"]
                                                                                 [show-dialogue "\"COIN AMOUNT + 1,000,000\""]
                                                                                 [show-dialogue "You finally have the means to escape!"]
                                                                                 [game-loop state-3])]
                                                                              )})]
        [list "Upgrade ?  (1x UPGRADE MODULE R)" "E" (lambda (game-state) {begin
                                                                            (cond
                                                                              [(not (item-in-inventory? "UPGRADE MODULE R " game-state))
                                                                               [show-dialogue "You don't have this upgrade yet"]
                                                                               [game-loop game-state]]

                                                                              [else
                                                                               (let* ([state-1 (remove-menu-item "main-menu" "P" game-state)]
                                                                                      [state-2 (add-menu-item state-1 "main-menu" "Press the coin button" "P" (lambda (game-state) {lambda-extractor game-state "logic for upgrade module r"}))]
                                                                                      [state-3 (use-item "UPGRADE MODULE R " state-2)]
                                                                                      [state-4 (remove-menu-item "upgrade-menu" "E" state-3)])
                                                                                 [show-dialogue "You insert the strange upgrade module into the coin generator"]
                                                                                 [show-dialogue "On the screen, you read: \"R MODE ACTIVATED\""]
                                                                                 [show-dialogue "You will now have a 30% chance of receiving 0.2x your total coins\nper press of the button; this bonus caps at 1,000 coins"]
                                                                                 [game-loop state-4])]
                                                                              )})]
        [list "Upgrade ?? (1x UPGRADE MODULE R2)" "F" (lambda (game-state) {begin
                                                                            (cond
                                                                              [(not (item-in-inventory? "UPGRADE MODULE R2 " game-state))
                                                                               [show-dialogue "You don't have this upgrade yet"]
                                                                               [game-loop game-state]]

                                                                              [(not (item-used? "UPGRADE MODULE R " game-state))
                                                                               [show-dialogue "You need UPGRADE MODULE R installed for this one to work"]
                                                                               [game-loop game-state]]

                                                                              [else
                                                                               (let* ([state-1 (remove-menu-item "main-menu" "P" game-state)]
                                                                                      [state-2 (add-menu-item state-1 "main-menu" "Press the coin button" "P" (lambda (game-state) {lambda-extractor game-state "logic for upgrade module r2"}))]
                                                                                      [state-3 (use-item "UPGRADE MODULE R2 " state-2)]
                                                                                      [state-4 (remove-menu-item "upgrade-menu" "F" state-3)])
                                                                                 [show-dialogue "You insert the strange upgrade module into the coin generator"]
                                                                                 [show-dialogue "On the screen, you read: \"R MODE BOOSTED\""]
                                                                                 [show-dialogue "You will now have a 40% chance of receiving 0.2x your total coins\nper press of the button; this bonus now caps at 30,000 coins"]
                                                                                 [game-loop state-4])]
                                                                              )})]
        ))

; --- NORTH PATH MENU -------------------------------------------------------------------------------

(define north-path
  (list "north-path"
        [list "Check your surroundings" "S" (lambda (game-state)
                                              {begin
                                                [show-dialogue "The chamber this passage has led you to is small and cramped.  To your left you are surpised to see a vending machine. Maybe you can get some food before you starve.\nTo your right there is a locked passage"]
                                                [define state-1 (add-menu-item game-state "north-path" "Use vending machine (7 coins)" "A" (lambda (game-state)
                                                                                                                                             {begin
                                                                                                                                               (cond
                                                                                                                                                 [(< (get-coin-count game-state) 7)
                                                                                                                                                  [show-dialogue "You don't have enough coins to use the vending machine.\nMaybe you should go back to the main area to generate some more"]
                                                                                                                                                  [game-loop game-state]]

                                                                                                                                                 [else
                                                                                                                                                  (let* ([state-1 (remove-menu-item "north-path" "A" game-state)]
                                                                                                                                                         [state-2 (deduct-coins 7 state-1)])
                                                                                                                                                    [show-dialogue "Your stomach growls as you insert your coins into the vending machine."]
                                                                                                                                                    [show-dialogue "After a few seconds of anticipation, your item drops to the vending machine receptacle.  You eagarly pick it up\nonly to realize it is just a dumb key.  You quietly curse to yourself"]
                                                                                                                                                    [game-loop (pick-up-item "KEY " state-2)])
                                                                                                                                                  ]
                                                                                                                                                 )}))]
                                                [define state-2 (add-menu-item state-1 "north-path" "Unlock passage (1 key)" "B" (lambda (game-state)
                                                                                                                                   {begin
                                                                                                                                     (cond
                                                                                                                                       [(item-in-inventory? "KEY " game-state)
                                                                                                                                        (let* ([state-1 (use-item "KEY " game-state)]
                                                                                                                                               [state-2 (remove-menu-item "north-path" "B" state-1)]
                                                                                                                                               [state-3 (add-menu-item state-2 "north-path" "Enter passage" "B" (lambda (game-state)
                                                                                                                                                                                                                  {begin
                                                                                                                                                                                                                    [show-dialogue "The passage leads you to a small room with a terminal at its center.\nA message on the terminal reads \"Play number game to win coin generator upgrade module.\"  You decide to give it a try"]
                                                                                                                                                                                                                    [define win? (number-guessing-game 100)]
                                                                                                                                                                                                                    (cond
                                                                                                                                                                                                                      [win?
                                                                                                                                                                                                                       (let* ([state-1 (pick-up-item "UPGRADE MODULE V1 " game-state)]
                                                                                                                                                                                                                              [state-2 (remove-menu-item "north-path" "B" state-1)])
                                                                                                                                                                                                                         [show-dialogue "The ground starts shaking around you"]
                                                                                                                                                                                                                         [show-dialogue "You quickly jump out from the passage before it collapses behind you.  \"Well, that was close!\""]
                                                                                                                                                                                                                         [show-dialogue "You should use the upgrade you just found on your coin generator"]
                                                                                                                                                                                                                         [game-loop state-2])]

                                                                                                                                                                                                                      [else
                                                                                                                                                                                                                       [show-dialogue "You angrily stomp out of the room in defeat"]
                                                                                                                                                                                                                       [game-loop game-state]]
                                                                                                                                                                                                                      )}))]
                                                                                                                                               )
                                                                                                                                          [show-dialogue "You use the key to unlock the passage"]
                                                                                                                                          [game-loop state-3]
                                                                                                                                          )]
                                                                                                                                       
                                                                                                                                       [else
                                                                                                                                        [show-dialogue "You stare helplessly at the locked passage hoping that by some miracle it will open"]
                                                                                                                                        [show-dialogue "After staring for what seems like an eternity, you give up"]
                                                                                                                                        [game-loop game-state]]
                                                                                                                                       )}
                                                                                                                                   ))]
                                                [define state-3 (add-menu-item state-2 "north-path" "Drop item" "W" (lambda (game-state) {begin
                                                                                                                              [define state-1 (drop-item game-state)]
                                                                                                                              [game-loop state-1]}))]
                                                [define state-4 (remove-menu-item "north-path" "S" state-3)]
                                                [game-loop state-4]
                                                })]
        [list "Go back to main area" "R" (lambda (game-state) {lambda-extractor game-state "logic for returning to main-menu"})]
        [list "Info" "Z" (lambda (game-state) {lambda-extractor game-state "logic for showing info"
                                                                #:current-location "NORTH PATH"
                                                                #:description "a small area with a vending machine and a passage that may or may not be locked"})]
        ))

; --- SOUTH PATH MENU ---------------------------------------------------------------------------------------------

(define south-path
  (list "south-path"
        [list "Check your surroundings" "S" (lambda (game-state)
                                              {begin
                                                [show-dialogue "The section of the cave system that this passage has led you to is utterly massive.  When attempting to look for the edges of the chamber, all you see is pitch black"]
                                                [show-dialogue "Searching the immediate vicinity, you see what appears to be a 3x3 formation of treasure chests.  To the right, there is another terminal"]
                                                [define state-1 (remove-menu-item "south-path" "S" game-state)]
                                                [define state-2 (add-menu-item state-1 "south-path" "Play chest game (1 mimic key)" "A" (lambda (game-state) {begin
                                                                                                                                                               (cond
                                                                                                                                                                 
                                                                                                                                                                 [(item-in-inventory? "MIMIC KEY " game-state)
                                                                                                                                                                  [show-dialogue "A sign next to the chests reads:\n\"Mimics always lie; real chests always tell the truth; find all real chests to win\""]
                                                                                                                                                                  [let ([win? (mimic-game-1)]
                                                                                                                                                                        [state-1 (use-item "MIMIC KEY " game-state)])
                                                                                                                                                                    
                                                                                                                                                                    (cond
                                                                                                                                                                      [win? [let* ([state-2 (remove-menu-item "south-path" "A" state-1)]
                                                                                                                                                                                   [state-3 (add-coins 1000 state-2)])                                                                                                  
                                                                                                                                                                              (show-dialogue "Within the chests, you found 1000 coins and an upgrade module for your coin generator!")
                                                                                                                                                                              (game-loop (pick-up-item "UPGRADE MODULE V2 " state-3))]]
                                                                                                                                                                      
                                                                                                                                                                      [else [let ([state-2 (set-coin-count 0 state-1)])
                                                                                                                                                                              (show-dialogue "The mimic jumps on top of you and starts trying to eat you.  Luckily, you manage to distract it by thowing all of your coins away allowing you to barely escape")
                                                                                                                                                                              (show-dialogue (format "-~a coins" (get-coin-count game-state)))
                                                                                                                                                                              (game-loop state-2)]]
                                                                                                                                                                      )]]

                                                                                                                                                                 [else
                                                                                                                                                                  [show-dialogue "You try and pry open each of the chests to no avail.  It seems you need some sort of key"]
                                                                                                                                                                  [game-loop game-state]]
                                                                                                                                                                 
                                                                                                                                                                 )}))]
                                                                                                                                          
                                                [define state-3 (add-menu-item state-2 "south-path" "Investigate the terminal" "B" (lambda (game-state) {begin
                                                                                                                                                          [show-dialogue "You read the screen of the terminal"]
                                                                                                                                                          [show-dialogue "\"Pay 30 coins to play memory game.  Prize: mimic key\""]

                                                                                                                                                          (cond
                                                                                                                                                            
                                                                                                                                                            [(< (get-coin-count game-state) 30)
                                                                                                                                                             [show-dialogue "Better go back to the coin generator to get some more coins"]
                                                                                                                                                             [game-loop game-state]]

                                                                                                                                                            [else
                                                                                                                                                             [show-dialogue "You decide to play the game and insert 30 coins into the terminal"]
                                                                                                                                                             [let ([win? (number-memorization-game)]
                                                                                                                                                                   [state-1 (deduct-coins 30 game-state)])
                                                                                                                                                               (cond
                                                                                                                                                                 [win? (game-loop (pick-up-item "MIMIC KEY " state-1))]

                                                                                                                                                                 [else
                                                                                                                                                                  [show-dialogue "You kick the machine in frustration and angrily stomp away to vent"]
                                                                                                                                                                  [game-loop state-1]])
                                                                                                                                                             ]])}))]
                                                                                                                                                            
                                                [define state-4 (add-menu-item state-3 "south-path" "Try exploring the darkness" "C" (lambda (game-state) {begin
                                                                                                                                                            [show-dialogue "You decide to try your luck exploring the darkness.  Who knows what you will find"]
                                                                                                                                                            [define rand-num (random 10)]

                                                                                                                                                            (cond
                                                                                                                                                              [(< rand-num 5)
                                                                                                                                                               (let* ([coin-amount (random 50)]
                                                                                                                                                                      [state-1 (add-coins coin-amount game-state)])
                                                                                                                                                                 [show-dialogue (format "You found ~a coins!" coin-amount)]
                                                                                                                                                                 [game-loop state-1])]

                                                                                                                                                              [(= rand-num 5)
                                                                                                                                                               [show-dialogue "You didn't find anything"]
                                                                                                                                                               [game-loop game-state]]

                                                                                                                                                              [(and (< rand-num 8) (not (item-collected? "UPGRADE MODULE R " game-state)))
                                                                                                                                                                 [show-dialogue "You found a strange upgrade module!"]
                                                                                                                                                                 [game-loop (pick-up-item "UPGRADE MODULE R " game-state)]]

                                                                                                                                                              [(item-in-inventory? "LANTERN " game-state)
                                                                                                                                                               (let* ([state-1 (use-item "LANTERN " game-state)]
                                                                                                                                                                      [state-2 (remove-menu-item "treasure-room" "M" state-1)]
                                                                                                                                                                      [state-3 (add-menu-item state-2 "south-path" "Enter secret passage" "E" (lambda (game-state) {lambda-extractor game-state "logic for entering secret passage found by lantern"}))])
                                                                                                                                                                 [show-dialogue "With the help of the lantern, you find a secret passage!"]
                                                                                                                                                                 [show-dialogue "You leave the lantern at the entrance to mark its location"]
                                                                                                                                                                 [game-loop state-3])]

                                                                                                                                                              [(= rand-num 9)
                                                                                                                                                               (let* ([coin-amount (random 1000)]
                                                                                                                                                                      [state-1 (add-coins coin-amount game-state)])
                                                                                                                                                                 [show-dialogue (format "You found ~a coins! Jackpot!" coin-amount)]
                                                                                                                                                                 [game-loop state-1])]

                                                                                                                                                              [else
                                                                                                                                                               (let* ([state-1 (set-coin-count 0 game-state)])
                                                                                                                                                                 [show-dialogue "A giant spider attacks you!  You lose all of your coins trying to escape."]
                                                                                                                                                                 [game-loop state-1])])}))]
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                                 

                                                                                                                               
                                                                                                                                                              
                                                                                                                                                               

                                                [define state-5 (add-menu-item state-4 "south-path" "Drop item" "W" (lambda (game-state) {begin
                                                                                                                           [define state-1 (drop-item game-state)]
                                                                                                                           [game-loop state-1]}))]
                                                [game-loop state-5]
                                                })]         
        [list "Go back to main area" "R" (lambda (game-state) {lambda-extractor game-state "logic for returning to main-menu"})]
        [list "Info" "Z" (lambda (game-state) {lambda-extractor game-state "logic for showing info"
                                                                #:current-location "SOUTH PATH"
                                                                #:description "an expansive chamber of the cave system; it is so massive you can't even see the other side"})]
        ))

; --- WEST PATH MENU -----------------------------------------------------------------------------------------

(define west-path
  (list "west-path"
        [list "Check your surroundings" "S" (lambda (game-state) {begin
                                                                   [show-dialogue "This path leads you to what is seemingly an endless hallway"]
                                                                   [show-dialogue "After walking for hours, you finally enter a room.  Examining the room, you spot a giant monster guarding what looks like an upgrade module for the coin generator\nAt the back of the room, you see a passage with a sign above it reading \"ENTER LABYRINTH\""]
                                                                   [define state-1 (add-menu-item game-state "west-path" "Fight the monster" "F" (lambda (game-state) {lambda-extractor game-state "logic for fighting the monster"}))]                                                                          
                                                                   [define state-2 (add-menu-item state-1 "west-path" "Enter the labyrinth" "A" (lambda (game-state) {lambda-extractor game-state "logic for entering labyrinth"}))]
                                                                   [define state-3 (remove-menu-item "west-path" "S" state-2)]
                                                                   [define state-4 (add-menu-item state-3 "west-path" "Drop item" "W" (lambda (game-state) {lambda-extractor game-state "logic for dropping items"}))]
                                                                   ;[define state-5 (add-menu-item state-4 "west-path" "Look at labyrinth map" "M" (lambda (game-state) {lambda-extractor game-state "logic for looking at labyrinth map"}))]
                                                                   [game-loop state-4]
                                                                   })]
                                                                   
        [list "Go back to main area" "R" (lambda (game-state) {lambda-extractor game-state "logic for returning to main-menu"})]
        [list "Info" "Z" (lambda (game-state) {lambda-extractor game-state "logic for showing info"
                                                                #:current-location "WEST PATH"
                                                                #:description "A room at the end of an absurdly long hallway with the entrance to a labyrinth at its back"})]
        ))

; --- LABYRINTH --------------------------------------------------------------------------


(define labyrinth-1
  (list "labyrinth-1"
        [list "Go north" "A" (lambda (game-state) {trap-game "You bump into a wall and activate a pressure plate triggering a boulder to fall on you" game-state "west-path"})]
        [list "Go east" "B" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "labyrinth-2" game-state)]
                                                   [game-loop state-1]})]
        [list "Go south" "C" (lambda (game-state) {trap-game "You bump into a wall, triggering a trap door to open underneath you. You to fall into a quicksand pit" game-state "west-path"})]
        [list "Go west" "D" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "west-path" game-state)]
                                                   [show-dialogue "You escape the labyrinth"]
                                                   [game-loop state-1]})]
        ))

(define labyrinth-2
  (list "labyrinth-2"
        [list "Go north" "A" (lambda (game-state) {trap-game "You bump into a wall.  The wall starts trying to eat you" game-state "west-path"})]
        [list "Go east" "B" (lambda (game-state) {trap-game "You reach a dead end.  On your way back, you step on a bear trap" game-state "west-path"})]
        [list "Go south" "C" (lambda (game-state) {begin
                                                    [define state-1 (set-current-menu "labyrinth-3" game-state)]
                                                    [game-loop state-1]})]
        [list "Go west" "D" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "labyrinth-1" game-state)]
                                                   [game-loop state-1]})]
        ))

(define labyrinth-3
  (list "labyrinth-3"
        [list "Go north" "A" (lambda (game-state) {begin
                                                    [define state-1 (set-current-menu "labyrinth-2" game-state)]
                                                    [game-loop state-1]})]
        [list "Go east" "B" (lambda (game-state) {begin
                                                    [define state-1 (set-current-menu "labyrinth-4" game-state)]
                                                    [game-loop state-1]})]
        [list "Go south" "C" (lambda (game-state) {trap-game "A giant spider snatches you and wraps you in their web" game-state "west-path"})]
        [list "Go west" "D" (lambda (game-state) {trap-game "A slight nudge against the wall activates a magical glyph, casting a web of ensnaring tendrils that bind you in place" game-state "west-path"})]
        ))

(define labyrinth-4
  (list "labyrinth-4"
        [list "Go north" "A" (lambda (game-state) {begin
                                                    [define state-1 (set-current-menu "labyrinth-2" game-state)]
                                                    [show-dialogue "You accidentally walk into a portal and are sent to a previous part of the labyrinth"]
                                                    [game-loop state-1]})]
        [list "Go east" "B" (lambda (game-state) {trap-game "You step on a magical glyph and are frozen solid" game-state "west-path"})]
        [list "Go south" "C" (lambda (game-state) {begin
                                                    [define state-1 (set-current-menu "labyrinth-5" game-state)]
                                                    [game-loop state-1]})]
        [list "Go west" "D" (lambda (game-state) {begin
                                                    [define state-1 (set-current-menu "labyrinth-3" game-state)]
                                                    [game-loop state-1]})]
        ))

(define labyrinth-5
  (list "labyrinth-5"
        [list "Go north" "A" (lambda (game-state) {begin
                                                    [define state-1 (set-current-menu "labyrinth-4" game-state)]
                                                    [game-loop state-1]})]
        [list "Go east" "B" (lambda (game-state) {begin
                                                    [define state-1 (set-current-menu "labyrinth-1" game-state)]
                                                    [show-dialogue "You accidentally walk into a portal and are sent to a previous part of the labyrinth"]
                                                    [game-loop state-1]})]
        [list "Go south" "C" (lambda (game-state) {begin
                                                    [define state-1 (set-current-menu "treasure-room" game-state)]
                                                    [show-dialogue "You enter the treasure room"]
                                                    [game-loop state-1]})]
        [list "Go west" "D" (lambda (game-state) {begin
                                                    [define state-1 (set-current-menu "labyrinth-2" game-state)]
                                                    [show-dialogue "You accidentally walk into a portal and are sent to a previous part of the labyrinth"]
                                                    [game-loop state-1]})]
        ))

(define treasure-room
  (list "treasure-room"
        [list "Open the treasure!" "O" (lambda (game-state) {lambda-extractor game-state "logic for finding out you have to play another mimic game"})]
        [list "Head back into the labyrinth" "A" (lambda (game-state) {begin
                                                                        [define state-1 (set-current-menu "labyrinth-5" game-state)]
                                                                        [show-dialogue "You head back into the labyrinth"]
                                                                        [game-loop state-1]})]
        ))

; --- DEEP LABYRINTH --------------------------------------------------------------------------

(define deep-labyrinth-1
  (list "deep-labyrinth-1"
        [list "Go north" "A" (lambda (game-state) {trap-game "You hit a wall and trip a pressure plate.  A giant cardboard box falls from the ceiling right on top of you" game-state "west-path"})]
        [list "Go east" "B" (lambda (game-state) {trap-game "You run face-first into a wall.  Dazed, you fall backwards onto a massive glue trap" game-state "west-path"})]
        [list "Go south" "C" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-2" game-state)]
                                                   [game-loop state-1]})]
        [list "Go west" "D" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "treasure-room" game-state)]
                                                   [show-dialogue "You head back into the treasure room"]
                                                   [game-loop state-1]})]
        ))

(define deep-labyrinth-2
  (list "deep-labyrinth-2"
        [list "Go north" "A" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-1" game-state)]
                                                   [game-loop state-1]})]
        [list "Go east" "B" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-3" game-state)]
                                                   [game-loop state-1]})]
        [list "Go south" "C" (lambda (game-state) {trap-game "You encounter a wall with a frozen pipe protruding from it.  Due to a lapse in your judgement, you decide to lick the pipe" game-state "west-path"})]
        [list "Go west" "D" (lambda (game-state) {trap-game "You find a trail of cheese. Hungrily following it and eating as you go, you end up caught in a giant mouse trap" game-state "west-path"})]
        ))

(define deep-labyrinth-3
  (list "deep-labyrinth-3"
        [list "Go north" "A" (lambda (game-state) {trap-game "You step on a pressure plate which causes the floor to fall out from beneath you. You fall into a crocodile infested pit" game-state "west-path"})]
        [list "Go east" "B" (lambda (game-state) {trap-game "You accidentally walk into a part of the labyrinth under construction.  Not watching your step, you end up stepping in wet cement that magically hardens as you touch it" game-state "west-path"})]
        [list "Go south" "C" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-4" game-state)]
                                                   [game-loop state-1]})]
        [list "Go west" "D" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-2" game-state)]
                                                   [game-loop state-1]})]
        ))

(define deep-labyrinth-4
  (list "deep-labyrinth-4"
        [list "Go north" "A" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-3" game-state)]
                                                   [game-loop state-1]})]
        [list "Go east" "B" (lambda (game-state) {trap-game "You slip on a banana peel and fall into a pit" game-state "west-path"})]
        [list "Go south" "C" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-5" game-state)]
                                                   [game-loop state-1]})]
        [list "Go west" "D" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "dead-end-1" game-state)]
                                                   [game-loop state-1]})]
        ))

(define deep-labyrinth-5
  (list "deep-labyrinth-5"
        [list "Go north" "A" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-4" game-state)]
                                                   [game-loop state-1]})]
        [list "Go east" "B" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-6" game-state)]
                                                   [game-loop state-1]})]
        [list "Go south" "C" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "dead-end-2" game-state)]
                                                   [game-loop state-1]})]
        [list "Go west" "D" (lambda (game-state) {trap-game "You accidentally trip a Rube Goldberg machine.  Mesmerized by its complexity, you fail to react to the falling anvil" game-state "west-path"})]
        ))

(define deep-labyrinth-6
  (list "deep-labyrinth-6"
        [list "Go north" "A" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-7" game-state)]
                                                   [game-loop state-1]})]
        [list "Go east" "B" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "dead-end-2" game-state)]
                                                   [game-loop state-1]})]
        [list "Go south" "C" (lambda (game-state) {trap-game "You are sucked up by a giant vacuum cleaner.  The dustbag you fall into is full and stomach acid and partialy digested creatures" game-state "west-path"})]
        [list "Go west" "D" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-5" game-state)]
                                                   [game-loop state-1]})]
        ))

(define deep-labyrinth-7
  (list "deep-labyrinth-7"
        [list "Go north" "A" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-8" game-state)]
                                                   [game-loop state-1]})]
        [list "Go east" "B" (lambda (game-state) {trap-game "A vengeful sprit possesses you and starts to make you punch yourself in the face.  \"Stop hitting yourself!\"" game-state "west-path"})]
        [list "Go south" "C" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-6" game-state)]
                                                   [game-loop state-1]})]
        [list "Go west" "D" (lambda (game-state) {trap-game "Strings descend from the ceiling, tying themselves around your arms and legs.  They then start making you dance like a puppet" game-state "west-path"})]
        ))

(define deep-labyrinth-8
  (list "deep-labyrinth-8"
        [list "Go north" "A" (lambda (game-state) {trap-game "You fall into a massive ball pit" game-state "west-path"})]
        [list "Go east" "B" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-9" game-state)]
                                                   [game-loop state-1]})]
        [list "Go south" "C" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-7" game-state)]
                                                   [game-loop state-1]})]
        [list "Go west" "D" (lambda (game-state) {trap-game "You run into a wall of flesh.  It grabs you with one of its tendrils and starts trying to incorporate you into itself" game-state "west-path"})]
        ))

(define deep-labyrinth-9
  (list "deep-labyrinth-9"
        [list "Go north" "A" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-10" game-state)]
                                                   [game-loop state-1]})]
        [list "Go east" "B" (lambda (game-state) {trap-game "You stumble across a shiny red button with a sign above it saying \"do not press.\" You press the button anyway which releases paralyzing fumes" game-state "west-path"})]
        [list "Go south" "C" (lambda (game-state) {trap-game "A giant block of gellatin falls on you, trapping you inside" game-state "west-path"})]
        [list "Go west" "D" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-8" game-state)]
                                                   [game-loop state-1]})]
        ))

(define deep-labyrinth-10
  (list "deep-labyrinth-10"
        [list "Go north" "A" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-11" game-state)]
                                                   [game-loop state-1]})]
        [list "Go east" "B" (lambda (game-state) {trap-game "You find a magical flower. You are getting down to smell it when a carnivorous plant snatches you up" game-state "west-path"})]
        [list "Go south" "C" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-9" game-state)]
                                                   [game-loop state-1]})]
        [list "Go west" "D" (lambda (game-state) {trap-game "You find a button by a sign that says \"free treasure.\" Pressing it causes a trap door to open above you showering you in adhesive slime" game-state "west-path"})]
        ))

(define deep-labyrinth-11
  (list "deep-labyrinth-11"
        [list "Go north" "A" (lambda (game-state) {trap-game "You find a chest. You tentatively open it only for it to be full of spring-loaded snakes that shoot out everywhere. The snakes then start ensaring you" game-state "west-path"})]
        [list "Go east" "B" (lambda (game-state) {trap-game "You find a potion and excitedly gulp it down.  You are temporarily transformed into a blobfish" game-state "west-path"})]
        [list "Go south" "C" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-10" game-state)]
                                                   [game-loop state-1]})]
        [list "Go west" "D" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-12" game-state)]
                                                   [game-loop state-1]})]
        ))

(define deep-labyrinth-12
  (list "deep-labyrinth-12"
        [list "Go north" "A" (lambda (game-state) {trap-game "You look down and see a penny.  You greedily try to snatch it up, only for the ground beneath you to open up into an edritch maw which starts trying to devour you" game-state "west-path"})]
        [list "Go east" "B" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-11" game-state)]
                                                   [game-loop state-1]})]
        [list "Go south" "C" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-treasure-room" game-state)]
                                                   [show-dialogue "You enter the deep treasure room"]
                                                   [game-loop state-1]})]
        [list "Go west" "D" (lambda (game-state) {trap-game "You step on a magical sigil which causes you to become magnetic.  Giant magnetic blocks start hurtling towards you from all directions" game-state "west-path"})]
        ))

(define dead-end-1
  (list "dead-end-1"
        [list "Go north" "A" (lambda (game-state) {trap-game "You fall into a towering glass box which starts to fill rapidly with water" game-state "west-path"})]
        [list "Go east" "B" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-4" game-state)]
                                                   [game-loop state-1]})]
        [list "Go south" "C" (lambda (game-state) {trap-game "You fall on a slip-n'-slide which dumps you into a tar pit" game-state "west-path"})]
        [list "Go west" "D" (lambda (game-state) {trap-game "An arm manifests from the ground.  It grabs you and won't let go" game-state "west-path"})]
        ))

(define dead-end-2
  (list "dead-end-2"
        [list "Go north" "A" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-5" game-state)]
                                                   [game-loop state-1]})]
        [list "Go east" "B" (lambda (game-state) {trap-game "You disturb a massive nest of hymneopteran insects.  They start swarming you" game-state "west-path"})]
        [list "Go south" "C" (lambda (game-state) {trap-game "You trigger a trip wire which activates a hidden mechanism releasing enchanted butterflies. The butterflies start shooting you with paralyzing bolts" game-state "west-path"})]
        [list "Go west" "D" (lambda (game-state) {trap-game "Stepping on what seemed to be an ordinary puddle, you realize it was actually super glue when you cannot lift your feet" game-state "west-path"})]
        ))

(define dead-end-3
  (list "dead-end-3"
        [list "Go north" "A" (lambda (game-state) {trap-game "An innocuous-looking lever triggers a series of mechanisms that transform the room into a giant pinball machine.\nHelplessly ricocheting off bumpers and flippers, you find yourself trapped in a chaotic game of chance, bouncing from obstacle to obstacle." game-state "west-path"})] ; from ChatGPT
        [list "Go east" "B" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "labyrinth-5" game-state)]
                                                   [show-dialogue "You accidentally step on a magical sigil which teleports you to a random part of the labyrinth"]
                                                   [game-loop state-1]})]
        [list "Go south" "C" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "labyrinth-1" game-state)]
                                                   [show-dialogue "You decide to take a nap. When you awake, you're in a different part of the labyrinth"]
                                                   [game-loop state-1]})]
        [list "Go west" "D" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-6" game-state)]
                                                   [game-loop state-1]})]
        ))

(define deep-treasure-room
  (list "deep-treasure-room"
        [list "Return to labyrinth" "R" (lambda (game-state) {begin
                                                   [define state-1 (set-current-menu "deep-labyrinth-12" game-state)]
                                                   [show-dialogue "You head back into the labyrinth"]
                                                   [game-loop state-1]})]
        [list "Pick up ???" "P" (lambda (game-state) {lambda-extractor game-state "logic for picking up upgrade module r2 from deep treasure room"})]
        ))

; --- SECRET PASSAGE --------------------------------------------------------------------------

(define secret-passage
  (list "secret-passage"
        [list "Pick up the sword" "A" (lambda (game-state) {lambda-extractor game-state "logic for picking up the sword"})]
        [list "Read the message" "B" (lambda (game-state) {lambda-extractor game-state "logic for reading the message"})]
        [list "Go back" "R" (lambda (game-state) {lambda-extractor game-state "logic for leaving secret passage"})]
        ))

; --- EAST PATH --------------------------------------------------------------------------------

(define east-path
  (list "east-path"
        [list "Check your surroundings" "S" (lambda (game-state) {begin
                                                                   (let* ([state-1 (add-menu-item game-state "east-path" "Play blackjack (100,000 coins)" "A" (lambda (game-state)
                                                                                                                                                                {lambda-extractor game-state "logic for playing blackjack"}))]
                                                                          [state-2 (add-menu-item state-1 "east-path" "Play number guessing game (50,000 coins)" "B" (lambda (game-state)
                                                                                                                                                                          {lambda-extractor game-state "logic for playing unfair number guessing game"}))]
                                                                          [state-3 (add-menu-item state-2 "east-path" "Play mimic game (1 mimic key)" "C" (lambda (game-state)
                                                                                                                                                               {lambda-extractor game-state "logic for playing casino mimic game"}))]
                                                                          [state-4 (add-menu-item state-3 "east-path" "Play pattern recognition game (150,000 coins)" "D" (lambda (game-state)
                                                                                                                                                            {lambda-extractor game-state "logic for playing pattern recognition game"}))]
                                                                          [state-5 (add-menu-item state-4 "east-path" "Play slot machine (50,000 coins)" "E" (lambda (game-state)
                                                                                                                                                               {lambda-extractor game-state "logic for playing slot machine"}))]
                                                                          [state-6 (remove-menu-item "east-path" "S" state-5)]
                                                                          [state-7 (add-menu-item state-6 "east-path" "Drop item" "W" (lambda (game-state) {lambda-extractor game-state "logic for dropping items"}))]
                                                                          [state-8 (add-menu-item state-7 "east-path" "Buy mimic key (150,000 coins)" "K" (lambda (game-state) {lambda-extractor game-state "logic for buying mimic key"}))])
                                                                     [show-dialogue "As you turn the corner, you are shocked to see a sprawling casino with all sorts of things to do"]
                                                                     [show-dialogue "Among all the blackjack tables and slot machines, surely you will be able to gather enough coins to leave this place"]
                                                                     [game-loop state-8])})]
        [list "Return to main area" "R" (lambda (game-state) {lambda-extractor game-state "logic for returning to main-menu"})]
        [list "Info" "Z" (lambda (game-state) {lambda-extractor game-state "logic for showing info"
                                                                #:current-location "EAST PATH"
                                                                #:description "An enormous subterranean casino of mysterious origin"})]
        ))

; --- VICTORY PATH ----------------------------------------------------------------------

(define victory-path
  (list "victory-path"
        [list "Check your surroundings" "S" (lambda (game-state) {begin
                                                                 (let* ([state-1 (add-menu-item game-state "victory-path" "Play high-stakes mimic game (1 mimic key)" "A" (lambda (game-state) {lambda-extractor game-state "logic for playing final mimic game"}))]
                                                                        [state-2 (add-menu-item state-1 "victory-path" "Escape (1 exit key)" "B" (lambda (game-state) {lambda-extractor game-state "logic for good ending"}))]
                                                                        [state-3 (add-menu-item state-2 "victory-path" "Drop item" "W" (lambda (game-state) {lambda-extractor game-state "logic for dropping items"}))]
                                                                        [state-4 (remove-menu-item "victory-path" "S" state-3)])
                                                                   [show-dialogue "You take a look around the alcove"]
                                                                   [show-dialogue "You see a door with an exit sign. You ecstatically run over to it only to realize it won't budge. It must be locked"]
                                                                   [show-dialogue "It is only after this that you realize there is yet another mimic game. Completing it must reward you the key you need to escape"]
                                                                   [game-loop state-4])})]
        [list "Go back" "R" (lambda (game-state) {lambda-extractor game-state "logic for returning to main-menu"})]
        [list "Info" "Z" (lambda (game-state) {lambda-extractor game-state "logic for showing info"
                                                                #:current-location "VICTORY PATH"
                                                                #:description "A small alcove with only a single obstacle between you and your escape. Don't mess this up!"})]
        ))

; the starting game state and its constituent parts
(define menu-list (list title main-menu upgrade-menu north-path south-path west-path east-path
                        labyrinth-1 labyrinth-2 labyrinth-3 labyrinth-4 labyrinth-5 treasure-room secret-passage
                        deep-labyrinth-1 deep-labyrinth-2 deep-labyrinth-3 deep-labyrinth-4 deep-labyrinth-5
                        deep-labyrinth-6 deep-labyrinth-7 deep-labyrinth-8 deep-labyrinth-9 deep-labyrinth-10
                        deep-labyrinth-11 deep-labyrinth-12 dead-end-1 dead-end-2 dead-end-3 deep-treasure-room
                        victory-path
                        ))

(define inventory-list (list
                        [list "KEY "               #f #f #f]
                        [list "UPGRADE MODULE V1 " #f #f #f]
                        [list "UPGRADE MODULE V2 " #f #f #f]
                        [list "UPGRADE MODULE V3 " #f #f #f]
                        [list "UPGRADE MODULE V4 " #f #f #f]
                        [list "UPGRADE MODULE R "  #f #f #f]
                        [list "UPGRADE MODULE R2 " #f #f #f]
                        [list "MIMIC KEY "         #f #f #f]
                        [list "LANTERN "           #f #f #f]
                        [list "SWORD "             #f #f #f]
                        [list "EXIT KEY "          #f #f #f]
                        ))

(define starting-game-state (list "title"
                                  menu-list
                                  inventory-list
                                  0 1))


; pre  -- takes a game-state object and a string key (and possibly some keyword arguments)
; post -- allows the lambda body of menu-items and add-menu-item function calls to be defined
;         in this function as opposed to being nested in a menu-item's definition
; signature: (game-state, string) -> lambda body
(define (lambda-extractor game-state key
                          #:current-location [current-location "n/a"]
                          #:description [description "n/a"]
                          #:num-id [num-id 0])
  (cond

; --- MAIN MENU ---

    ; the lambda body for unlocking victory-path
    [(equal? key "logic for unlocking victory-path")

     (cond
       [(< (get-coin-count game-state) 20000000)
        [show-dialogue "You don't have quite enough coins to do this yet"]
        [game-loop game-state]]

       [else
        (let* ([state-1 (deduct-coins 20000000 game-state)]
               [state-2 (remove-menu-item "main-menu" "E" state-1)]
               [state-3 (add-menu-item state-2 "main-menu" "Enter the alcove" "E" (lambda (game-state) {lambda-extractor game-state "logic for entering victory-path"}))])
          [show-dialogue "Using your millions of coins, you build yourself a staircase of gold to the top of the chamber"]
          [show-dialogue "At the top, there is an alcove. That must be the way out!"]
          [game-loop state-3])]
       )]

    ; the lambda body for entering victory-path
    [(equal? key "logic for entering victory-path")

     (let* ([state-1 (set-current-menu "victory-path" game-state)])
       [show-dialogue "You climb your staircase and enter the alcove"]
       [game-loop state-1])]

; --- VICTORY PATH ---

    ; the lambda body for getting the good ending
    [(equal? key "logic for good ending")

     (cond
       [(not (item-in-inventory? "EXIT KEY " game-state))
        [show-dialogue "You try the door again, but it won't budge. Looks like the mimic game is the only way"]
        [game-loop game-state]]

       [else
        [show-dialogue "You use the key to open the door and are immediately hit with sunlight. You did it!"]
        [show-dialogue "You got the good ending"]
        [game-loop starting-game-state]]
       )]

    ; the lambda body for playing the final mimic game
    [(equal? key "logic for playing final mimic game")

     (cond
       [(not (item-in-inventory? "MIMIC KEY " game-state))
        [show-dialogue "You need a mimic key to play"]
        [game-loop game-state]]

       [else
        (let ([win? (mimic-game-4)]
              [state-0 (use-item "MIMIC KEY " game-state)])
          (cond
            [win?
             (let* ([state-1 (remove-menu-item "victory-path" "A" state-0)]
                    [state-2 (add-coins 2147483647 state-1)])
               [show-dialogue "Within the chests, you find 2,147,483,647 coins and the exit key"]
               [game-loop (pick-up-item "EXIT KEY " state-2)]
               )]

            [else
             [show-dialogue "The mimic and its two friends tackle you and start devouring you"]
             [show-dialogue "As you lie on the ground dying, you can only think of how close you were to escaping"]
             [show-dialogue "You got the bad ending"]
             [game-loop starting-game-state]]
            ))]
       )]
               
     
    
; --- UPGRADE MENU ---

    ; the lambda body of the add-menu-item call when using UPGRADE MODULE R from the upgrade-menu
    [(equal? key "logic for upgrade module r")
     
     (let* ([rand-num (random 10)]
            [state-1 (click game-state)])
       (cond
         [(>= rand-num 7)
          (let* ([bonus (floor (/ (get-coin-count game-state) 5))]
                 [adjusted-bonus (min bonus 1000)]
                 [state-2 (add-coins adjusted-bonus state-1)])
            [show-dialogue (format "You recieved ~a bonus coins; you now have ~a coins" adjusted-bonus (get-coin-count state-2))]
            [game-loop state-2])]

         [else
          [game-loop state-1]]
         ))]

    ; the lambda body of the add-menu-item call when using UPGRADE MODULE R2 from the upgrade-menu
    [(equal? key "logic for upgrade module r2")

     (let* ([rand-num (random 10)]
            [state-1 (click game-state)])
       (cond
         [(>= rand-num 6)
          (let* ([bonus (floor (/ (get-coin-count game-state) 5))]
                 [adjusted-bonus (min bonus 30000)]
                 [state-2 (add-coins adjusted-bonus state-1)])
            [show-dialogue (format "You recieved ~a bonus coins; you now have ~a coins" adjusted-bonus (get-coin-count state-2))]
            [game-loop state-2])]

         [else
          [game-loop state-1]]
         ))]

; --- SOUTH-PATH ---
    
    ; the lambda body for entering the secret passage
    [(equal? key "logic for entering secret passage found by lantern")

     (let* ([state-1 (set-current-menu "secret-passage" game-state)])
       [show-dialogue "You enter the secret passage"]
       [show-dialogue "Looking around, you see a sword and a message engraved into the wall"]
       [game-loop state-1])]

; --- SECRET-PASSAGE ---
    
    ; the lambda body for picking up the sword
    [(equal? key "logic for picking up the sword")

     (let* ([state-1 (pick-up-item "SWORD " game-state)]
            [state-2 (remove-menu-item "secret-passage" "A" state-1)])
       [game-loop state-2])]

    ; the lambda body for reading the message for the first time in the secret passage
    [(equal? key "logic for reading the message")
     
     (let* ([state-1 (remove-menu-item "secret-passage" "B" game-state)]
            [state-2 (add-menu-item state-1 "secret-passage" "Re-read the message" "B" (lambda (game-state) {lambda-extractor game-state "logic for rereading message"}))]
            [state-3 (add-menu-item state-2 "treasure-room" "Look for the secret" "S" (lambda (game-state) {lambda-extractor game-state "logic for looking for secret in treasure room"}))])
       [show-dialogue "\"A secret can be found in the treasure room of the labyrinth.  Look for a hidden switch\""]
       [game-loop state-3]
       )]

    ; the lambda body for reading the messsage any subsequent time in the secret passage
    [(equal? key "logic for rereading message")
     
     [show-dialogue "\"A secret can be found in the treasure room of the labyrinth.  Look for a hidden switch\""]
     [game-loop game-state]]

    ; the lambda body for leaving the secret passage
    [(equal? key "logic for leaving secret passage")

     (let* ([state-1 (set-current-menu "south-path" game-state)])
       [show-dialogue "You leave the secret passage"]
       [game-loop state-1])]

; --- WEST PATH ---

    ; the lambda body of the add-menu-item call when choosing to fight the monster in west-path
    [(equal? key "logic for fighting the monster")

     (cond                                                                                                                                                                  
       [(and (item-in-inventory? "LANTERN " game-state) (not (item-in-inventory? "SWORD " game-state)))
        (let* ([labyrinth-loc-list (list "labyrinth-1" "labyrinth-2" "labyrinth-3" "labyrinth-4" "labyrinth-5")]
               [random-location (list-ref labyrinth-loc-list (random (length labyrinth-loc-list)))]
               [state-1 (add-menu-item game-state random-location "Pick up the lantern" "P" (lambda (game-state) {lambda-extractor game-state "logic for picking up the thrown lantern"}))]
               [state-2 (use-item "LANTERN " state-1)])
          (show-dialogue "You bravely approach the monster with your new lantern")
          (show-dialogue "The monster, unimpressed, grabs your lantern and throws it to a random location in the labyrinth")
          (game-loop state-2)
          )]
       
       [(not (item-in-inventory? "SWORD " game-state))
        (let* ([coin-amount (random 2000)]
               [state-1 (deduct-coins coin-amount game-state)])
          [display "Pick a number between 1 and 3\n   >>>"]
          [c-read-line]
          [show-dialogue "You harmlessly punch the monster. \"Take that!\""]
          [show-dialogue "The monster, unfazed, beats you to a pulp and steals some of your coins"]
          [show-dialogue (format "-~a coins" coin-amount)]
          [game-loop state-1])]

       [else
        (let* ([state-1 (remove-menu-item "west-path" "F" game-state)])
          [show-dialogue "The monster sees that you're armed and runs away, leaving the upgrade module behind"]
          [show-dialogue "You do a little victory dance and pick up the upgrade module"]
          [game-loop (pick-up-item "UPGRADE MODULE V3 " state-1)])]
       )]

    ; the lambda body for picking up the lantern thrown by the monster
    [(equal? key "logic for picking up the thrown lantern")

     (let* ([state-1 (pick-up-item "LANTERN " game-state)]
            [state-2 (remove-menu-item (get-current-menu-name state-1) "P" state-1)])
       [show-dialogue "\"Why did I think that would work?\" you think out loud.\nThere must be some other dark area where you can use the lantern"]
       [game-loop state-2])]
     

    ; the lambda body of the add-menu call that adds an option for looking at the labyrinth map
    [(equal? key "logic for looking at labyrinth map")

     [show-dialogue "Using the light of the lantern, you see what appears to be a map engraved on the walls"]
     [show-dialogue
"++++++++++++++++++++++++++++++
++++++++++++++++++++++++++++++
###############+++++++++++++++
++++++++++##++++++++++++++++++
++++++++++##+++++##+++++++++++
++++++++++##+++++##+++++++++++
++++++++++##+++++##+++++++++++
++++++++++##############++++++
++++++++++##+++++##+++++++++++
++++++++++##+++++##+++++++++++
++++++++++##+++++##+++++++++++
++++++++++##+++++##+++++++++++
+++++++++++++++++##+++++++++++
+++++++++++++++++##+++++++++++
+++++++++++++++++##+++++++++++
+++++++++++++++++##+++++++++++"]
     [game-loop game-state]]

    ; the lambda body for handling the logic for entering the labyrinth
    [(equal? key "logic for entering labyrinth")

     (let* ([state-1 (set-current-menu "labyrinth-1" game-state)])
       [show-dialogue "You cautiously enter the labyrinth"]
       [show-dialogue "Inside, it is too dark to see anything. Looks like it is going to be trial and error from here on out"]
       [game-loop state-1])]

; --- TREASURE ROOM ---
    
    ; the lambda body of the "open the chests!" option from the treasure room of the labyrinth
    [(equal? key "logic for finding out you have to play another mimic game")

     (let* ([state-1 (remove-menu-item "treasure-room" "O" game-state)]
            [state-2 (add-menu-item state-1 "treasure-room" "Play mimic game (1 mimic key)" "O" (lambda (game-state) {lambda-extractor game-state "logic for playing second mimic game"}))])
       [show-dialogue "You greedily try to open the chests to no avail.  It is only after this that you notice the sign. \"Play Mimic Game 2 Electric Boogaloo, Only One Mimic Key!\""]
       [show-dialogue "\"#%&*$$#!!!\" Time to head back to south-path to get another mimic key"]
       [game-loop state-2])]

    ; the lambda body for the logic of the second mimic game
    [(equal? key "logic for playing second mimic game")

     (cond
       [(not (item-in-inventory? "MIMIC KEY " game-state))
        [show-dialogue "You need a mimic key from the memorization game to play"]
        [game-loop game-state]]

       [else
        (let ([win? (mimic-game-2)]
              [state-1 (use-item "MIMIC KEY " game-state)])
          (cond
            [win?
             (let* ([state-2 (remove-menu-item "treasure-room" "O" state-1)]
                    [state-3 (add-coins 10000 state-2)]
                    [state-4 (add-menu-item state-3 "treasure-room" "Use lantern to look at wall glyphs" "M" (lambda (game-state) {lambda-extractor game-state "logic for looking at labyrinth map"}))])
               [show-dialogue "Within the chests, you found 10,000 coins and a lantern!"]
               [game-loop (pick-up-item "LANTERN " state-4)])]

            [else
             [show-dialogue "The mimic feels bad for you and gives you a 15,000 coin consolation prize"]
             [game-loop (add-coins 15000 state-1)]]
            ))]
       )]

    ; the lambda body for looking for the secret alluded to by the message in the secret-passage
    [(equal? key "logic for looking for secret in treasure room")

     (let* ([state-1 (remove-menu-item "treasure-room" "S" game-state)]
            [state-2 (add-menu-item state-1 "treasure-room" "Go deeper into labyrinth" "L" (lambda (game-state) {lambda-extractor game-state "logic for going deeper into labyrinth"}))])
       [show-dialogue "Not wanting to put up with this stupid labyrinth any more, you frantically search every nook and cranny in the room"]
       [show-dialogue "Finally, you find a hidden switch. Upon flipping it, a passage leading deeper into the labyrinth opens"]
       [game-loop state-2])]

    ; the lambda body for entering the deep-labyrinth
    [(equal? key "logic for going deeper into labyrinth")

     (let ([state-1 (set-current-menu "deep-labyrinth-1" game-state)])
       [show-dialogue "You reluctantly head deeper into the labyrinth"]
       [game-loop state-1])]

; --- DEEP TREASURE ROOM ---

    ; lambda body for picking up the item in the deep treasure room
    [(equal? key "logic for picking up upgrade module r2 from deep treasure room")

     (let* ([state-1 (remove-menu-item "deep-treasure-room" "P" game-state)])
       [game-loop (pick-up-item "UPGRADE MODULE R2 " state-1)])]
            

; --- MISCELLANEOUS ---

    ; the lambda body of the menu-item for showing game info found in the five main areas
    [(equal? key "logic for showing info")

     [display-inventory game-state]
     [displayln "  ---"]
     [displayln (format "   COINS: ~a" (get-coin-count game-state))]
     [displayln "  ---"]
     [displayln (format "   CURRENT LOCATION: ~a" current-location)]
     [displayln "  ---"]
     [display (format "   BRIEF DESCRIPTION: ~a" description)]
     [show-dialogue ""]
     [game-loop game-state]
     ]

    ; the lambda body of the menu-item for dropping items found in the five main areas
    [(equal? key "logic for dropping items")

     [define state-1 (drop-item game-state)]
     [game-loop state-1]]

    ; the lambda body for returning to the main area
    [(equal? key "logic for returning to main-menu")

     (let ([state-1 (set-current-menu "main-menu" game-state)])
       [show-dialogue "You head back to the main area"]
       [game-loop state-1])]

; --- EAST PATH ---

    ; the lambda body for playing blackjack
    [(equal? key "logic for playing blackjack")

     (cond
       [(< (get-coin-count game-state) 100000)
        [show-dialogue "You do not have enough coins. Go generate some more"]
        [game-loop game-state]]

       [else
        [show-dialogue "You give 100,000 coins to the dealer and take a seat"]
        (let* ([state-1 (deduct-coins 100000 game-state)]
               [temp (bs-blackjack)])
          [show-dialogue "You can't help but feel you have been cheated"]
          [game-loop state-1])])]

    ; the lambda body for playing the unfair number guessing game
    [(equal? key "logic for playing unfair number guessing game")

     (cond
       [(< (get-coin-count game-state) 50000)
        [show-dialogue "You're too poor to play"]
        [game-loop game-state]]

       [else
        [show-dialogue "You decide to fork over 50,000 coins to play the number guessing game"]
        (let ([win? (number-guessing-game 1000000)]
              [state-1 (deduct-coins 50000 game-state)])
          (cond
            [win?
             [show-dialogue "You miraculously won! You receive 10,000,000 coins!"]
             [game-loop (add-coins 10000000 state-1)]]

            [else
             [show-dialogue "You feel cheated out of your coins"]
             [game-loop state-1]]
            ))]
       )]

    ; the lambda body for playing the casino mimic game
    [(equal? key "logic for playing casino mimic game")

     (cond
       [(not (item-in-inventory? "MIMIC KEY " game-state))
        [show-dialogue "You need a mimic key to play"]
        [game-loop game-state]]

       [else
        [show-dialogue "You decide to play the mimic game"]
        (let* ([state-1 (use-item "MIMIC KEY " game-state)]
               [state-2 (deduct-coins 30000 state-1)]
               [win (mimic-game-3)])
          [show-dialogue "The mimic works for the casino and charges you 30,000 coins for playing"]
          [game-loop state-2])]
       )]

    ; the lambda body for playing the bogus pattern recognition game
    [(equal? key "logic for playing pattern recognition game")

     (cond
       [(< (get-coin-count game-state) 150000)
        [show-dialogue "You don't have enough coins to play"]
        [game-loop game-state]]

       [else
        [show-dialogue "You decide to take a shot at the pattern game"]
        (begin
          [unfair-number-game]
          [game-loop (deduct-coins 150000 game-state)])]
       )]

    ; the lambda body for playing the slot machine
    [(equal? key "logic for playing slot machine")

     [show-dialogue "You insert 50,000 coins into the machine"]
     (let ([win? (slot-machine)]
           [state-1 (deduct-coins 50000 game-state)])
       (cond
         [win?
          [show-dialogue "An upgrade module is dispensed from the machine. You pick it up"]
          [game-loop (pick-up-item "UPGRADE MODULE V4 " state-1)]]

         [else
          [game-loop state-1]]
         ))]

    ; the lambda body that handles the logic of buying mimic keys
    [(equal? key "logic for buying mimic key")

     (cond
       [(< (get-coin-count game-state) 150000)
        [show-dialogue "You don't have enough coins"]
        [game-loop game-state]]

       [(item-in-inventory? "MIMIC KEY " game-state)
        [show-dialogue "You already have a mimic key"]
        [game-loop game-state]]

       [else
        (let* ([state-1 (deduct-coins 150000 game-state)])
          [show-dialogue "You give the vendor 150,000 coins"]
          [game-loop (pick-up-item "MIMIC KEY " state-1)])]
       )]
       
    

; --- KEY NOT FOUND ---
    [else
     [show-dialogue (format "Error: not yet implemented; key: \"~a\"" key)]
     [game-loop game-state]]

  ))