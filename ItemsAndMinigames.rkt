#lang racket

(require "GameState.rkt") ; provides functions for manipulating game-state

(provide inventory-list)

(define inventory-list (list
                        [list "COIN GENERATOR " #f #f #f]
                        [list "USELESS KEEPSAKE " #t #t #f]
                        [list "KEY " #f #f #f]
                        [list "todo" #f #f #f]
                        ))