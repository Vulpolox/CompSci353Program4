#lang racket

(provide pad-string)
(provide round-to-2)

; pre  -- takes a string "s," a character "c," an integer "t," and a string "a"
; post -- returns a padded string composed of the orignal string "s"
;         with a number of "c" added to the left or right (specified by "a") in order
;         for the resulting combined string's length to be "t" characters long;
;         possible values for "a" are "left" and "right"
(define (pad-string str char target-length align)

                                                   ; helper function that limits the size of a string and returns it
  (define (_trim-string str limit)
    (substring str 0 (min (string-length str) limit)))
  
  (define str-length (string-length str))          ; the length of str
  (define pad-amount (- target-length str-length)) ; the amount of char to add to the output

  (cond
    
    [(<= target-length str-length) (_trim-string str target-length)] ; if target-length is less than str-length, trim str to target-length and return that

    [else                                                            ; if target-length is greater than str-length
     (let* ([padding (make-string pad-amount char)]                  ; create the padding
            [padding-lst (string->list padding)]                     ; turn the padding into a list of characters
            [char-lst (string->list str)])                           ; turn the input str into a list of characters

       (cond

         [(equal? "left" align)                                      ; if align is "left," prepend the padding to the input and return the result
          (list->string (append padding-lst char-lst))]

         [(equal? "right" align)                                     ; if align is "right," append the padding to the input and return the result
          (list->string (append char-lst padding-lst))]

         [else "error: align must be left or right"]                 ; return an error message if an invalid value for align is passed
         ))]
    ))

; pre  -- takes a number
; post -- returns the number rounded to two decimal places as a string
(define (round-to-2 num)
  (~r #:precision '(= 2) num))