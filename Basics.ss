
; Append; adds param 'item' to the 
; last item of param 'target'. 
; param 'target' is usually a list

(define (append item target)
    (reverse
        (cons item (reverse target)
        )
    )
)

; List Index; list-ref takes the nth 
; item from a given list.

;(list-ref list n)

; Homegrown implementation of list-indexing 
; using only car, cdr and recursion

(define (index list n)
    (if (= n 0)
        (car list)
        (index (cdr list (- n 1))
        )
    )
)

; getValue; get the value of a Key-Value dotted pair
; where param 'key' is a string or a character and and param dict 
; is a list of dotted pairs.

(define (getValue key dict)
    (cond ((string? key)
            (if (string-ci=? key (car (car dict)))
                (cdr (car dict))
                (getValue key (cdr dict))
            )
        )
        
        ((char? key)
            (if (string-ci=? (string key) (car (car dict)))
                (cdr (car dict))
                (getValue (string key) (cdr dict))
            )
        )
    )
)

(define numerology 
    '(("a" . 1) ("b" . 2) ("c" . 3) ("d" . 4)
      ("e" . 5) ("f" . 6) ("g" . 7) ("h" . 8)
      ("i" . 9) ("j" . 10) ("k" . 20) ("l" . 30)
      ("m" . 40) ("n" . 50) ("o" . 60) ("p" . 70)
      ("q" . 80) ("r" . 90) ("s" . 100) ("t" . 200)
      ("u" . 300) ("v" . 400) ("w" . 500) ("x" . 600)
      ("y" . 700) ("z" . 800))
)

; index-str; gets the nth character in a given string
; param 'str' should be either a string or a list of characters
; param n should be an integer value

(define (index-str str n)
    (cond ((string? str) 
                (if (= n 0)
                    (car (string->list str))
                    (index-str (cdr (string->list str)) (- n 1))
                )
          )
          
        ((list? str)
            (if (= n 0)
                (car str)
                (index-str (cdr str) (- n 1))
            )
        )
    )
)

; Add one, param "int" should be a number
(define (++ int)
    (+ int 1)
)


; addval-list; get the value of the first
; value of a string, list, or that of a single char.
; param 'str' could be a list, string or char

(define (addval-list str target base-dict)
    (append 
        (getValue (car (string->list str))
            base-dict
        )              
        target
    )
)

; setType-list turns strings and chars into
; lists for use in recursion.

(define (setType-list item)
    (cond ((string? item)
            (string->list item)
        )
        
        ((list? item)
            item
        )
        
        ((char? item)
            (cons item '())
        )
    )
)

; getValList; turns the given param 'str' which could be 
; a string, char or list, into a list of values mapped against a 'dict'
; param target is usually an empty list, but could also be used to 
; add to a pre-existing ValList. 
; NOTE: make sure that it does not have whitespaces anywhere

(define (getValList str target dict)
    (if (equal? (cdr (setType-list str)) '()) 

        (cons (getValue (car (setType-list str)) dict)
              target
        )

        (getValList (cdr (setType-list str)) ; param str
            (cons (getValue (car (setType-list str)) dict) ; param target
                target
            )
            dict ; self-xplanatory :P
        )
    )
)

; whitesp-strip; rmoves all of the whitespace in a given string
(define (whitesp-strip str target)
    (cond ((equal? (cdr (setType-list str)) '())
            (cons (car (setType-list str)) target)
        )
        
        ((equal? (car (setType-list str)) '#\space)
            (whitesp-strip (cdr (setType-list str))
                target
            )
        )

        (else (whitesp-strip
                (cdr (setType-list str))
                (cons (car (setType-list str))
                    target
                )
              )
        )
    )
)


; sumlist; sums up all the numerical values in a list of numbers
; param 'target' should be zero unless one is adding up the value of 
; a different calculation

(define (sumlist ls target)
    (if (equal? (cdr ls) '())
        (+ (car ls) target)
        (sumlist 
            (cdr ls) 
            (+ (car ls) target)
        )
    )
)

; getNumVal; extracts the numerological equivalent of a given
; string value, param 'str'

(define (getNumVal str)
    (sumlist 
        (getValList 
            (whitesp-strip str '()) 
             '()
             numerology
        )
        0
    )
)

; exponentate; homegrown implementation of expt; I try my best to remember only
; the bare minimum of scheme operating code so I can learn to implement the other
; stuff myself; params 'num' and 'pow' are numbers
(define (exponentate num pow)
    (if (= pow 1)
        num
        (* num (exponentate num (- pow 1))
        )
    )
)


