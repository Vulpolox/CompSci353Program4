#lang racket

(require "Menus.rkt")             ; provides "menu-list" containing all of the game's menus
(require "GameState.rkt")         ; provides the "game-loop" function
(require "ItemsAndMinigames.rkt") ; provides the "inventory-list" containing all items in the game

(define starting-game-state (list "title"
                                  menu-list
                                  inventory-list
                                  1000 0))

(game-loop starting-game-state)