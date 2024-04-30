#lang racket

(require "GameState.rkt") ; provides functions for manipulating game-state
(require "UI.rkt")
(require "Format.rkt")

(provide number-guessing-game)
(provide number-memorization-game)
(provide trap-game)
(provide mimic-game-1)
(provide mimic-game-2)
(provide bs-blackjack)
(provide unfair-number-game)

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
    (define guess (c-read-line))

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
       (displayln "The correct number is higher")
       (loop correct-number (- guesses-left 1))]
      
      [(> (string->number guess) correct-number)               ; player's guess is greater than the correct number
       (displayln "The correct number is lower")
       (loop correct-number (- guesses-left 1))]
      ))
  
  (loop correct-number))

; --- NUMBER MEMORIZATION MINIGAME -----------------------------------------------------------------------------------

(define random-facts ; from ChatGPT prompt "can you generate me a list of random interesting facts?"
  [list "Did you know: The shortest war in history was between Britain and Zanzibar on August 27, 1896. It lasted only 38 minutes."
        "Did you know: Octopuses have three hearts."
        "Did you know: Honey never spoils. Archaeologists have found pots of honey in ancient Egyptian tombs that are over 3,000 years old and still perfectly edible."
        "Did you know: The Eiffel Tower can be 15 cm taller during the summer due to thermal expansion."
        "Did you know: Bananas are berries, but strawberries aren't."
        "Did you know: The shortest complete sentence in the English language is \"I am.\""
        "Did you know: The dot over the letter \"i\" is called a tittle."
        "Did you know: The national animal of India is the Bengal tiger."
        "Did you know: The only continent with no active volcanoes is Australia."
        "Did you know: The longest English word without a vowel is \"rhythms.\""
   ])

(define (number-memorization-game)
  (define num-1 (number->string (+ (random 90000) 10000))) ; random number between 10,000 and 99,999
  (define num-2 (number->string (+ (random 90000) 10000))) ; random number between 10,000 and 99,999
  (define num-list (list num-1 num-2))
  (define random-fact (list-ref random-facts [random (length random-facts)]))
  (define index-of-num-to-memorize [random (length num-list)])
  (define num-to-memorize (list-ref num-list index-of-num-to-memorize))

  (define (clear-console [lines-to-print 5000])
    (if [= lines-to-print 0]
        [displayln "------------------------------------------"]
        [begin (displayln "*")
               (clear-console (- lines-to-print 1))
        ]
        ))

  (displayln "------------------------------------------")
  (show-dialogue "Memorize the following information:")
  (clear-console)
  (show-dialogue (format "Number #1: ~a" num-1))
  (clear-console)
  (show-dialogue (format "Number #2: ~a" num-2))
  (clear-console)
  (show-dialogue random-fact)
  (clear-console)
  (display (format "What was number #~a?~n   >>>" (add1 index-of-num-to-memorize)))
  (define answer (c-read-line))

  (if [equal? answer num-to-memorize]
      [begin (displayln "You win!\n------------------------------------------") #t]
      [begin (displayln (format "Number was ~a; You lose :(~n------------------------------------------" num-to-memorize)) #f]
      ))

; --- TRAP MINI GAME -------------------------------------------------------------------------------------------------


; pre  -- takes a string message and a game-state object, and a menu-name
; post -- displays the string message and then has the player
;         play a number guessing game until either they guess the correct
;         number or run out of coins; changes the game-states current-menu if they lose
; signature: (string, game-state, menu-name) -> game-loop call
(define (trap-game trap-message game-state lose-location)
  (define current-coins (get-coin-count game-state))
  (show-dialogue trap-message)

  (define (trap-game-loop current-coins [range 10])
    (define correct-number (add1 (random range)))     ; random number between 1 and range
    (define coin-lose-amount (random 2000))           ; the amount of coins you lose if you guess incorrectly

    (cond
      [(< current-coins 0)                            ; lose condition: you're in debt; send player to lose-location
       (show-dialogue "You have run out of coins. You pass out")
       (show-dialogue (format "When you awaken, you find yourself at the entrance to the labyrinth" lose-location))
       (let* ([state-1 (set-coin-count 0 game-state)]
              [state-2 (set-current-menu lose-location game-state)])
         [game-loop state-2])]
      
      [else                                           ; otherwise, get a guess from the player and compare it against different cases
       (display (format "Guess the correct number between 1 and ~a to escape the trap~n   >>>" range))
       (let ([guess (c-read-line)])

         (cond
           
           [(or (boolean? (string->number guess))     ; guess isn't a number or is out of range; punish the player for bad input
                (> (string->number guess) range)
                (< (string->number guess) 1))
            (show-dialogue "Your guess was so bad that you triggered another trap making your escape harder; you lost double coins as well")
            (show-dialogue (format "-~a coins; you now have ~a coins" (* coin-lose-amount 2) (- current-coins (* 2 coin-lose-amount))))
            (trap-game-loop [- current-coins (* 2 coin-lose-amount)] [min (+ 5 range) 25])]

           [(= (string->number guess) correct-number) ; win condition (guess == correct-number); call game-loop with a game-state that has current-coins coins
            (show-dialogue "You escape the trap!")
            (let ([state-1 (set-coin-count current-coins game-state)])
              [game-loop state-1])]

           [else                                      ; player doesn't guess correctly; call the game loop with a decreased number of coins
            (show-dialogue (format "You struggle while trying your best to get out of the trap but fail~n-~a coins; you now have ~a coins~nCorrect number: ~a" coin-lose-amount (- current-coins coin-lose-amount) correct-number))
            (trap-game-loop (- current-coins coin-lose-amount) (max 5 (- range 1)))]
           
           ))]
      ))

  (trap-game-loop current-coins))
       
       
; --- BLACKJACK ------------------------------------------------------------------------------------------------------

; pre  -- takes no arguments
; post -- has the player play an unfair game of blackjack; returns true if game results in a tie, otherwise false
; signature: void -> boolean
(define (bs-blackjack)
  (blackjack-game-loop))

(define (blackjack-game-loop [current-total 0] [stand? #f])
  (cond
    [stand?
     (show-dialogue (format "Your final hand: ~a~nThe house's final hand: 21\n---\nYou lose!" current-total)) #f]

    [(> current-total 21)
     (show-dialogue (format "Current hand: ~a~nBust! You lose" current-total)) #f]

    [(= current-total 21)
     (show-dialogue "Tie! No one wins") #t]

    [else
     (displayln (format "Your current hand: ~a~n---" current-total))
     ((make-choice blackjack-menu) current-total)]))
     

(define blackjack-menu
  (list "blackjack-menu"
        [list "Hit" "A" (lambda (current-total) {begin
                                                  (let* ([card-amount (+ (add1 (random 10)) 10)]
                                                         [new-total (+ current-total card-amount)])
                                                    [show-dialogue (format "You drew a ~a.  \"What the heck?\"" card-amount)]
                                                    [blackjack-game-loop new-total #f])})]
        [list "Stand" "B" (lambda (current-total) {blackjack-game-loop current-total #t})]
        ))


; --- UNFAIR NUMBER GAME ----------------------------------------------------------------------------------------

; pre  -- takes no arguments
; post -- has the player play an unfair number game where there are no correct answers
; signature: void -> void
(define (unfair-number-game)

  (define unfair-number-menu
  (list "unfair-number-menu"
        [list (format "~a" (random 10)) "A" (lambda () {display ""})]
        [list (format "~a" (random 10)) "B" (lambda () {display ""})]
        [list (format "~a" (random 10)) "C" (lambda () {display ""})]
        [list (format "~a" (random 10)) "D" (lambda () {display ""})]
        ))
  
  (displayln (format "What is the next number in this sequence: ~a, ~a, ~a, ~a, ~a~n---"
                     (random 10) (random 10) (random 10) (random 10) (random 10)))
  ((make-choice unfair-number-menu))
  (show-dialogue "That was incorrect.  You lose!"))


  
    

; --- MIMIC GAME -----------------------------------------------------------------------------------------------------

; pre  -- takes a mimic-game-state (formatted as '(num-non-mimics-opened chest-menu chest-string total-num-non-mimics failed? num-mimics))
; post -- gets an option from chest-menu from user input and calls the state-manipulating function associated with it;
;         player loses if they are fooled into picking a mimic, they win if they find all non-mimics; mimics always lie, non-mimics always tell the truth;
;         if player loses, returns #f; if they win, returns #t
(define (mimic-game mimic-game-state)
  (define num-non-mimics-opened (first mimic-game-state))
  (define chest-menu (second mimic-game-state))
  (define chest-string (third mimic-game-state))
  (define total-num-non-mimics (fourth mimic-game-state))
  (define failed? (fifth mimic-game-state))
  (define num-mimics (sixth mimic-game-state))

  (cond
    [failed?
     (displayln "Uh oh! You opened a mimic!\n----------------------")
     #f]

    [(= num-non-mimics-opened total-num-non-mimics)
     (displayln "You successfully opened all chests!\n----------------------")
     #t]

    [else
     ((make-choice chest-menu) mimic-game-state)]
    ))

(define (open-mimic mimic-game-state)
  [list (first mimic-game-state) (second mimic-game-state)
        (third mimic-game-state) (fourth mimic-game-state) #t
        (sixth mimic-game-state)])

(define (open-non-mimic mimic-game-state)
  [show-dialogue "You open the chest and find some coins!"]
  [list (add1 (first mimic-game-state)) (second mimic-game-state)
        (third mimic-game-state) (fourth mimic-game-state)
        (fifth mimic-game-state) (sixth mimic-game-state)])

(define (remove-mimic-menu-item menu-choice mimic-game-state)
  (define menu-object (second mimic-game-state))
  (define menu-choices (cdr menu-object))
  (define menu-name (first menu-object))
  (define filtered-menu-items (filter [lambda (menu-item) (not (equal? (second menu-item) menu-choice))]
                                      menu-choices))
  (define updated-menu (append (list menu-name) filtered-menu-items))

  [list (first mimic-game-state) updated-menu
        (third mimic-game-state) (fourth mimic-game-state)
        (fifth mimic-game-state) (sixth mimic-game-state)])

(define (show-chests mimic-game-state)
  (show-dialogue (third mimic-game-state)))

(define template-chest-menu
  (list "chest-menu-1"
        [list "Pick chest A" "A" (lambda (original-state) "todo")]
        [list "Pick chest B" "B" (lambda (original-state) "todo")]
        [list "Pick chest C" "C" (lambda (original-state) "todo")]
        [list "Pick chest D" "D" (lambda (original-state) "todo")]
        [list "Pick chest E" "E" (lambda (original-state) "todo")]
        [list "Pick chest F" "F" (lambda (original-state) "todo")]
        [list "Pick chest G" "G" (lambda (original-state) "todo")]
        [list "Pick chest H" "H" (lambda (original-state) "todo")]
        [list "Pick chest I" "I" (lambda (original-state) "todo")]
        ))

; *** MIMIC GAME #1 ********************

(define chest-menu-1
  (list "chest-menu-1"
        [list "Pick chest A" "A" (lambda (original-state) {begin
                                                            [define state-1 (open-mimic original-state)]
                                                            [mimic-game state-1]})]
        [list "Pick chest B" "B" (lambda (original-state) {begin
                                                            [define state-1 (open-non-mimic original-state)]
                                                            [define state-2 (remove-mimic-menu-item "B" state-1)]
                                                            [mimic-game state-2]})]
        [list "Pick chest C" "C" (lambda (original-state) {begin
                                                            [define state-1 (open-non-mimic original-state)]
                                                            [define state-2 (remove-mimic-menu-item "C" state-1)]
                                                            [mimic-game state-2]})]
        [list "Pick chest D" "D" (lambda (original-state) {begin
                                                            [define state-1 (open-mimic original-state)]
                                                            [mimic-game state-1]})]
        [list "Pick chest E" "E" (lambda (original-state) {begin
                                                            [define state-1 (open-non-mimic original-state)]
                                                            [define state-2 (remove-mimic-menu-item "E" state-1)]
                                                            [mimic-game state-2]})]
        [list "Pick chest F" "F" (lambda (original-state) {begin
                                                            [define state-1 (open-non-mimic original-state)]
                                                            [define state-2 (remove-mimic-menu-item "F" state-1)]
                                                            [mimic-game state-2]})]
        [list "Pick chest G" "G" (lambda (original-state) {begin
                                                            [define state-1 (open-mimic original-state)]
                                                            [mimic-game state-1]})]
        [list "Pick chest H" "H" (lambda (original-state) {begin
                                                            [define state-1 (open-non-mimic original-state)]
                                                            [define state-2 (remove-mimic-menu-item "H" state-1)]
                                                            [mimic-game state-2]})]
        [list "Pick chest I" "I" (lambda (original-state) {begin
                                                            [define state-1 (open-non-mimic original-state)]
                                                            [define state-2 (remove-mimic-menu-item "I" state-1)]
                                                            [mimic-game state-2]})]
        [list "Show Chests" "Z" (lambda (original-state) {begin
                                                           [show-chests original-state]
                                                           [mimic-game original-state]})]
        ))

(define chest-string-1
  "Number of Mimics: 3 || Number of Chests: 6

Chest: A                    Chest: B                    Chest: C
Color: Black                Color: Black                Color: Red
Message: there is no        Message: there is a         Message: the rightmost
         mimic among the             mimic among the             column contains
         black boxes                 blue boxes                  no mimics

Chest: D                    Chest: E                    Chest: F
Color: Blue                 Color: Blue                 Color: Blue
Message: the rightmost      Message: there is a         Message: the bottom row
         column has one              mimic among the             has at least
         or more mimics              blue boxes                  one mimic

Chest: G                    Chest: H                    Chest: I
Color: Blue                 Color: Black                Color: Black
Message: there is no        Message: there is no        Message: there is no
         mimic among                 mimic among                 mimic among
         the blue boxes              the red boxes               the red boxes
")

(define mimic-game-state-1 (list 0 chest-menu-1 chest-string-1 6 #f 3))

(define (mimic-game-1)
  (show-chests mimic-game-state-1)
  (mimic-game mimic-game-state-1))

; *** MIMIC GAME #2 ********************

(define chest-menu-2
  (list "chest-menu-1"
        [list "Pick chest A" "A" (lambda (original-state) {begin
                                                            [define state-1 (open-mimic original-state)]
                                                            [mimic-game state-1]})]
        [list "Pick chest B" "B" (lambda (original-state) {begin
                                                            [define state-1 (open-mimic original-state)]
                                                            [mimic-game state-1]})]
        [list "Pick chest C" "C" (lambda (original-state) {begin
                                                            [define state-1 (open-non-mimic original-state)]
                                                            [define state-2 (remove-mimic-menu-item "C" state-1)]
                                                            [mimic-game state-2]})]
        [list "Pick chest D" "D" (lambda (original-state) {begin
                                                            [define state-1 (open-non-mimic original-state)]
                                                            [define state-2 (remove-mimic-menu-item "D" state-1)]
                                                            [mimic-game state-2]})]
        [list "Pick chest E" "E" (lambda (original-state) {begin
                                                            [define state-1 (open-mimic original-state)]
                                                            [mimic-game state-1]})]
        [list "Pick chest F" "F" (lambda (original-state) {begin
                                                            [define state-1 (open-non-mimic original-state)]
                                                            [define state-2 (remove-mimic-menu-item "F" state-1)]
                                                            [mimic-game state-2]})]
        [list "Pick chest G" "G" (lambda (original-state) {begin
                                                            [define state-1 (open-non-mimic original-state)]
                                                            [define state-2 (remove-mimic-menu-item "G" state-1)]
                                                            [mimic-game state-2]})]
        [list "Show Chests" "Z" (lambda (original-state) {begin
                                                           [show-chests original-state]
                                                           [mimic-game original-state]})]
        ))

(define chest-string-2
  "Number of Mimics: 3 || Number of Chests: 4

                    Chest: A                              Chest: B
                    Message: The box on the               Message: Diagonal down
                             right isn't a                         left from me
                             mimic                                 is a mimic

Chest: C                             Chest: D                                      Chest: E
Message: Diagonal up                 Message: Diagonal down                        Message: The box
         right from me                        right from me                                 on the left
         is a mimic                           isn't a mimic                                 is a mimic
                    
                    Chest: F                               Chest: G
                    Message: The box                       Message: Diagonal up
                             on the right                           left from me
                             isn't a mimic                          isn't a mimic
")

(define mimic-game-state-2 (list 0 chest-menu-2 chest-string-2 4 #f 3))

(define (mimic-game-2)
  (show-chests mimic-game-state-2)
  (mimic-game mimic-game-state-2))
