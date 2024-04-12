#lang racket

(require "UI.rkt")
(require "Format.rkt")

; NOTES:
;   format of menus           : '(menu-name '(message-1 choice-1 function-1) ... '(message-n choice-n function-n))
;   format of items           : '(item-name collected? in-inventory? used?)
;   format of menu-lists      : '(menu-1 menu-1 ... menu-n)
;   format of inventory-lists : '(item-1 item-2 ... item-n)
;   format of game-states     : '(current-menu-name menu-list inventory-list coin-count click-amount)

(define example-item (list "holy spear " #t #t #f))
(define example-item2 (list "cursed goblet " #t #t #f))

(define menu1 
  (list "M1"
        [list "Go to menu 2" "A" (lambda (game-state) "Todo")]
        [list "Pick up items" "B" (lambda (game-state) "Todo")]
        [list "Do things" "C" (lambda (game-state) "Todo")]
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


; pre  -- takes a menu-name and a game-state object
; post -- returns the menu-object corresponding to the specified menu-name
;         within the game-state's menu-list
(define (find-menu menu-name game-state)
  (define menu-list (get-menu-list game-state))                          ; the menu-list of the game-state object
  (define target (filter [lambda (menu) (equal? (first menu) menu-name)] ; use filter to find the correct menu from menu-list
                         menu-list))
  (first target))                                                        ; return the desired menu

; pre  -- takes a menu-name, a message, a choice, a function, and a game-state object
; post -- returns a new game-state object with the specified message, choice, and function appended to the menu items section of the menu specified
;         by menu-name within the new game-state's menu-list (quite a mouthfull, I know)
(define (add-menu-item menu-name message choice func game-state)
  (define menu-item-to-add (list message choice func))                                                                            ; the menu item to add
  (define menu-to-update (find-menu menu-name game-state))                                                                        ; the menu to which the new menu item will be added
  (define updated-menu (append menu-to-update (list menu-item-to-add)))                                                           ; the menu with menu-item-to-add added to it
  (define old-menu-list (get-menu-list game-state))                                                                               ; the menu-list from the input game-state object
  (define old-menu-list-no-outdated-entry (filter [lambda (menu-object) (not (equal? (first menu-object) (first updated-menu)))]  ; the same menu-list minus the outdated menu
                                                  old-menu-list))
  (define updated-menu-list (append (list updated-menu) old-menu-list-no-outdated-entry))                                         ; the final, updated menu-list
  (set-menu-list updated-menu-list game-state))                                                                                   ; return a game-state object with the updated menu-list

; pre  -- takes a menu-name, a choice-to-remove, and a game-state object
; post -- returns a new game-state object with the menu-item removed
;         from the menu-object specified by the passed menu-name and choice-to-remove parameters in the new game-state object's menu-list 
(define (remove-menu-item menu-name choice-to-remove game-state)
  (define target-menu (find-menu menu-name game-state))
  (define updated-target-menu (filter [lambda (menu-item) (not (equal? (second menu-item) choice-to-remove))]
                                      [cdr target-menu]))
  (define fixed-updated-menu (append (list menu-name) updated-target-menu))
  (define old-menu-list (get-menu-list game-state))                                                                               
  (define old-menu-list-no-outdated-entry (filter [lambda (menu-object) (not (equal? (first menu-object) (first fixed-updated-menu)))]
                                                  old-menu-list))
  (define updated-menu-list (append (list fixed-updated-menu) old-menu-list-no-outdated-entry))
  (set-menu-list updated-menu-list game-state))


; --- INVENTORY-LIST FUNCTIONS --------------------------------------------

; pre  -- takes a game-state object
; post -- 
(define (display-inventory game-state)
  (define inventory-list (get-inventory-list game-state))
  (define owned-items (filter [lambda (item) (equal? (second item) #t)]
                              inventory-list))
  (displayln "ITEMS IN INVENTORY:")
  (for-each [lambda (item) (printf "   ~a x1~n" (pad-string (first item) #\. 30 "right"))]
            owned-items)
  (display "\n"))

; --- COIN FUNCTIONS -----------------------------------------------------


;(display-inventory example-game-state)
;(game-loop example-game-state)
