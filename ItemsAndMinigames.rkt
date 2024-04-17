#lang racket

(require "GameState.rkt") ; provides functions for manipulating game-state
(require "UI.rkt")
(require "Format.rkt")

(provide inventory-list)
(provide number-guessing-game)

; --- ITEMS / INVENTORY LIST --------------------------------------------------------------

(define inventory-list (list
                        [list "COIN GENERATOR " #f #f #f]
                        [list "USELESS KEEPSAKE " #t #t #f]
                        [list "KEY " #f #f #f]
                        [list "UPGRADE MODULE V1 " #f #f #f]
                        ))

; --- NUMBER GUESSING MINIGAME ---------------------------------------------------------------------------

; pre  -- takes an integer range
; post -- generates a random number between 0 and range and has has the user play a guessing game;
;         returns #t if the player wins, #f if they lose
(define (number-guessing-game range)
  (define correct-number (random (add1 range)))                ; random number between 1 and range

  (displayln "------------------------------------------")
  ;(displayln correct-number)

  (define (loop correct-number [guesses-left 5])               ; the gameplay loop of the guessing game
    (display (format "Guess a number between 1 and ~a.  Guesses left: ~a~n   >>>" range (add1 guesses-left)))
    (define guess (read-line))

    (cond
      [(< guesses-left 0)                                      ; lose condition #1
       (displayln (format "You lose; the correct number was ~a~n------------------------------------------" correct-number))
       #f]
      
      [(or (not (andmap char-numeric? [string->list guess]))   ; string is not a number
           (equal? (string-length guess) 0))
       (displayln "That's not a number, silly. You lose 2 guesses for that")
       (loop correct-number (- guesses-left 2))]
      
      [(= (string->number guess) correct-number)               ; win condition
       (displayln "You win!\n------------------------------------------")
       #t]

      [(= guesses-left 0)                                      ; lose condition #2
       (displayln (format "You lose; the correct number was ~a~n------------------------------------------" correct-number))
       #f]

      [(< (string->number guess) correct-number)               ; player's guess is less than the correct number
       (displayln "Your guess is lower than the number")
       (loop correct-number (- guesses-left 1))]
      
      [(> (string->number guess) correct-number)               ; player's guess is greater than the correct number
       (displayln "Your guess is higher than the number")
       (loop correct-number (- guesses-left 1))]
      ))
  
  (loop correct-number))

; --- NUMBER MEMORIZATION MINIGAME -----------------------------------------------------------------------------------

; --- MIMIC GAME -----------------------------------------------------------------------------------------------------

; pre  -- takes a mimic-game-state (formatted as '(num-non-mimics-opened chest-menu chest-string total-num-non-mimics failed?))
; post -- gets an option from chest-menu from user input and calls the state-manipulating function associated with it
;         player loses if they are fooled into picking a mimic, they win if they find all non-mimics; mimics always lie;
;         if player loses, return #f; if they win, return #t
(define (mimic-game mimic-game-state)
  (define num-non-mimics-opened (first mimic-game-state))
  (define chest-menu (second mimic-game-state))
  (define chest-string (third mimic-game-state))
  (define total-num-non-mimics (fourth mimic-game-state))
  (define failed? (fifth mimic-game-state))

  (cond
    [failed?
     (displayln "Uh oh! You opened a mimic!  The mimic eats all of your coins\n----------------------")
     #f]

    [(= num-non-mimics-opened total-num-non-mimics)
     (displayln "You successfully opened all chests!\n----------------------")
     #t]

    [else
     (displayln chest-string)
     ((make-choice chest-menu) mimic-game-state)]
    ))

(define chest-menu-1
  (list "chest-menu-1"
        [list "Pick chest A" "A" (lambda (original-state) "todo")]
        [list "Pick chest B" "A" (lambda (original-state) "todo")]
        [list "Pick chest C" "A" (lambda (original-state) "todo")]
        [list "Pick chest D" "A" (lambda (original-state) "todo")]
        [list "Pick chest E" "A" (lambda (original-state) "todo")]
        [list "Pick chest F" "A" (lambda (original-state) "todo")]
        [list "Pick chest G" "A" (lambda (original-state) "todo")]
        [list "Pick chest H" "A" (lambda (original-state) "todo")]
        [list "Pick chest I" "A" (lambda (original-state) "todo")]
        ))