#lang racket

(require "Format.rkt") ; provides "pad-string" function for string formatting

(provide make-choice)
(provide show-dialogue)
(provide valid?)

; --- MENU STUFF -----------------------------------------------------------------------------------------
; 
(define example-menu 
  (list "Menu Name"
        [list "Thing 1" "A" (lambda () "manipulate state")]
        [list "Thing 2" "B" (lambda () "manipulate state")]
        ))

(define (get-menu-name menu-object)
  (first menu-object))

(define (display-menu menu-object)
  (define sorted-menu-items (sort [cdr menu-object]
                                  [lambda (current next) (< [char->integer (string-ref (second current) 0)] [char->integer (string-ref (second next) 0)])]
                                  ))
  (displayln "PICK ONE OF THE FOLLOWING:")
  (for-each [lambda (menu-item)
                    (printf "   ~a -- ~a~n" (pad-string (second menu-item) #\space 1 "left")
                                            (pad-string (first menu-item) #\space 70 "right"))
            ]
            sorted-menu-items))

(define (get-valid-choices menu-object)
  (_get-at-index (cdr menu-object) 1))

(define (valid? choice menu-object)
  (not (equal? (member choice (get-valid-choices menu-object)) #f)))

(define (get-menu-func valid-choice menu-object)
  (define target (filter [lambda (menu-item) (equal? (second menu-item) valid-choice)]
                         [cdr menu-object]))
  (third (first target)))

(define (make-choice menu-object)
  (begin
    (display-menu menu-object)
    (display "   >>> ")
    (define unvalidated-choice (string-upcase (read-line)))

    (cond
      
      [(valid? unvalidated-choice menu-object)
       (get-menu-func unvalidated-choice menu-object)]

      [else
       (begin
         (displayln "Invalid Choice; Try Again\n---")
         (make-choice menu-object))]
      )))

; --- DIALOGUE STUFF ----------------------------------------------------------------------------------

(define (show-dialogue dialogue)
  (displayln dialogue)
  (display "---\nPRESS ENTER TO CONTINUE >>>")
  (define pause (read-line))
  (displayln "---"))
  
; --- HELPER FUNCTIONS --------------------------------------------------------------------------------
            
; pre  -- takes a list of lists and an integer index; all sublists must have at least index+1 elements
; post -- returns a list containing all elements of the sublists at the specified index
(define (_get-at-index lst index [output '()])

  (cond
    
    [(empty? lst)
     output]

    [else
     (let ([element-to-add (list-ref (first lst) index)])

       (_get-at-index (cdr lst) index (cons element-to-add output)))]))