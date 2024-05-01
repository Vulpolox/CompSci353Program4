#lang racket

(require "Menus.rkt")             ; provides "menu-list" containing all of the game's menus
(require "GameState.rkt")         ; provides the "game-loop" function

; format of inventory-item: '(item-name collected? in-inventory? used?)
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

; format of game-state: '(current-menu-name menu-list inventory-list coin-count click-amount)
(define starting-game-state (list "title"
                                  menu-list
                                  inventory-list
                                  0 1))

(game-loop starting-game-state)