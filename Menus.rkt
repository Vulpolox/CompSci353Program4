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
        [list "Drop item" "W" (lambda (game-state)
                                {begin
                                  [define state-1 (drop-item game-state)]
                                  [game-loop state-1]
                                  })]
        [list "Check your surroundings" "S" (lambda (game-state)
                                              {begin
                                                [show-dialogue "You take a look around. There are four blocked paths in each of the cardinal directions but no way to advance.\nOut of the corner of your eye, you spot a strange device"]
                                                [define state-1 (add-menu-item game-state "main-menu" "Investigate the strange device" "P" (lambda (game-state)
                                                                                                                                {begin
                                                                                                                                  [show-dialogue "Upon closer examination of the device, you find a button. Upon pressing it, a golden coin drops to the ground"]
                                                                                                                                  [define state-1 (remove-menu-item "main-menu" "P" game-state)]
                                                                                                                                  [define state-2 (increment-click-amount 1 state-1)]
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
                                                                               "todo"])})]
        [list "Upgrade 4  (1x UPGRADE MODULE V4)" "D" (lambda (game-state) {begin
                                                                            (cond
                                                                              [(not (item-in-inventory? "UPGRADE MODULE V4 " game-state))
                                                                               [show-dialogue "You don't have this upgrade yet"]
                                                                               [game-loop game-state]]

                                                                              [else
                                                                               "todo"])})]
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
                                                                               "todo"])})]
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
                                                                                                                                                                      [state-2 (add-menu-item state-1 "south-path" "Enter secret passage" "E" (lambda (game-state) {lambda-extractor game-state "logic for adding secret passage found by lantern"}))])
                                                                                                                                                                 [show-dialogue "With the help of the lantern, you find a secret passage!"]
                                                                                                                                                                 [game-loop state-2])]

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
                                                                   [define state-5 (add-menu-item state-4 "west-path" "Look at labyrinth map" "M" (lambda (game-state) {lambda-extractor game-state "logic for looking at labyrinth map"}))]
                                                                   [game-loop state-5]
                                                                   })]
                                                                   
        [list "Go back to main area" "R" (lambda (game-state) {lambda-extractor game-state "logic for returning to main-menu"})]
        [list "Info" "Z" (lambda (game-state) {lambda-extractor game-state "logic for showing info"
                                                                #:current-location "WEST PATH"
                                                                #:description "The entrance to a labyrinth"})]
        [list "Test" "T" (lambda (game-state) [trap-game "test" game-state])]
        ))

; menu-list to provide for use in the starting game-state
(define menu-list (list title main-menu upgrade-menu north-path south-path west-path))


; pre  -- takes a game-state object and a string key (and possibly some keyword arguments)
; post -- allows the lambda body of menu-items and add-menu-item function calls to be defined
;         in this function as opposed to being nested in a menu-item's definition
; signature: (game-state, string) -> lambda body
(define (lambda-extractor game-state key
                          #:current-location [current-location "n/a"]
                          #:description [description "n/a"]
                          #:num-id [num-id 0])
  (cond

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

; --- SOUTH-PATH ---
    
    ; the lambda body of the add-menu-item call for finding the secret passage using LANTERN in south-path
    [(equal? key "logic for adding secret passage found by lantern")
     "TODO"]

; --- WEST PATH ---

    ; the lambda body of the add-menu-item call when choosing to fight the monster in west-path
    [(equal? key "logic for fighting the monster")

     (cond                                                                                                                                                                  
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

    ; the lambda body of the add-menu call that adds an option for looking at the labyrinth map
    [(equal? key "logic for looking at labyrinth map")

     [show-dialogue "Next to the labyrinth entrance, there is what appears to be some sort of map engraved into the wall"]
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
    

; --- KEY NOT FOUND ---
    [else
     [show-dialogue (format "Error: key wasn't found; key: \"~a\"" key)]
     [game-loop game-state]]


  ))