#lang racket

(require "UI.rkt")
(require "Format.rkt")

(define example-item (list "item 1 " #t #t #f))
(define example-item2 (list "cursed goblet " #t #t #f))

(define menu1 
  (list "M1"
        [list "Go to menu 2" "A" (lambda (game-state) "Todo")]
        [list "Pick up items" "B" (lambda (game-state) "Todo")]
        ))

(define menu2
  (list "M2"
        [list "Go to menu 1" "A" (lambda (game-state) "todo")]
        ))

(define example-game-state (list "M1"
                                 [list menu1 menu2]
                                 [list example-item example-item2]
                                 0 1))

; --- GAME LOOP ------------------------------------------------------------

(define (game-loop game-state)
  (define current-menu (find-menu [get-current-menu-name game-state] game-state))
  ((make-choice current-menu) game-state))

; --- STATE GETTER FUNCTIONS ----------------------------------------------

(define (get-current-menu-name game-state)
  (first game-state))
(define (get-menu-list game-state)
  (second game-state))
(define (get-inventory-list game-state)
  (third game-state))
(define (get-coin-count game-state)
  (fourth game-state))
(define (get-click-amount game-state)
  (fifth game-state))


; --- STATE SETTER FUNCTIONS ----------------------------------------------

(define (set-current-menu new-menu-name game-state)
  (list new-menu-name (get-menu-list game-state) (get-inventory-list game-state) (get-coin-count game-state) (get-click-amount game-state)))
(define (set-menu-list new-menu-list game-state)
  (list (get-current-menu-name game-state) new-menu-list (get-inventory-list game-state) (get-coin-count game-state) (get-click-amount game-state)))
(define (set-inventory-list new-inventory-list game-state)
  (list (get-current-menu-name game-state) (get-menu-list game-state) new-inventory-list (get-coin-count game-state) (get-click-amount game-state)))
(define (set-coin-count new-coin-count game-state)
  (list (get-current-menu-name game-state) (get-menu-list game-state) (get-inventory-list game-state) new-coin-count (get-click-amount game-state)))
(define (set-click-amount new-click-amount game-state)
  (list (get-current-menu-name game-state) (get-menu-list game-state) (get-inventory-list game-state) (get-coin-count game-state) new-click-amount))

; --- MENU-LIST FUNCTIONS -------------------------------------------------

(define (find-menu menu-name game-state)
  (define menu-list (get-menu-list game-state))
  (define target (filter [lambda (menu) (equal? (first menu) menu-name)]
                         menu-list))
  (first target))

; --- INVENTORY-LIST FUNCTIONS --------------------------------------------

(define (display-inventory game-state)
  (define inventory-list (get-inventory-list game-state))
  (define owned-items (filter [lambda (item) (equal? (second item) #t)]
                              inventory-list))
  (displayln "ITEMS IN INVENTORY:")
  (for-each [lambda (item) (printf "   ~a x1~n" (pad-string (first item) #\. 30 "right"))]
            owned-items))

;(display-inventory example-game-state)

;(set-current-menu "M2" example-game-state)

(game-loop example-game-state)