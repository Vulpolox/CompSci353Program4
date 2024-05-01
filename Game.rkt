#lang racket

(require "Menus.rkt")             ; provides "menu-list" containing all of the game's menus
(require "GameState.rkt")         ; provides the "game-loop" function
(require "ItemsAndMinigames.rkt") ; provides the "inventory-list" containing all items in the game

; format of inventory-item: '(item-name collected? in-inventory? used?)
(define inventory-list (list
                        [list "COIN GENERATOR "    #f #f #f]
                        [list "KEY "               #f #f #f]
                        [list "UPGRADE MODULE V1 " #f #f #f]
                        [list "UPGRADE MODULE V2 " #f #f #f]
                        [list "UPGRADE MODULE V3 " #f #f #f]
                        [list "UPGRADE MODULE V4 " #f #f #f]
                        [list "UPGRADE MODULE R "  #f #f #f]
                        [list "UPGRADE MODULE R2 " #f #f #f]
                        [list "MIMIC KEY "         #f #f #f]
                        [list "LANTERN "           #f #t #f]
                        [list "SWORD "             #f #f #f]
                        ))

(define starting-game-state (list "title"
                                  menu-list
                                  inventory-list
                                  1000000000000 0))

(game-loop starting-game-state)