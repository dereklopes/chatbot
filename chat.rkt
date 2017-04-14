;;; CS 152 Homework 4 - A simple chatbot
;;; starter code

;;; We'll use the random function implemented in Racket
;;; (random k) returns a random integer in the range 0 to k-1
(#%require (only racket/base random))

;;; some input and output helper functions

;;; prompt:  prompt the user for input
;;; return the input as a list of symbols
(define (prompt)
   (newline)
   (display "talk to me >>>")
   (read-line))

;;; read-line: read the user input till the eof character
;;; return the input as a list of symbols
(define (read-line)
  (let ((next (read)))
    (if (eof-object? next)
        '()
        (cons next (read-line)))))

;;; output: take a list such as '(how are you?) and display it
(define (output lst)
       (newline)
       (display (to-string lst))
       (newline))

;;; to-string: convert a list such as '(how are you?)
;;; to the string  "how are you?"
(define (to-string lst)       
  (cond ((null? lst) "")
        ((eq? (length lst) 1) (symbol->string (car lst)))
        (else (string-append (symbol->string (car lst))
                              " "
                             (to-string (cdr lst))))))


;;;  main function
;;;  usage:  (chat-with 'your-name)

(define (chat-with name)
  (output (list 'hi name))
  (chat-loop name))

;;; chat loop
(define (chat-loop name)
  (let ((input (prompt))) ; get the user input
    (if (eqv? (car input) 'bye)
        (begin
          (output (list 'bye name))
          (output (list 'have 'a 'great 'day!)))
        (begin
	  (reply input name)
          (chat-loop name)))))


;;; your task is to fill in the code for the reply function
;;; to implement rules 1 through 11 with the required priority
;;; each non-trivial rule must be implemented in a separate function
;;; define any helper functions you need below
(define (reply input name)
  (cond ((testInputRule1 input) (rule1 input name))
        ((not (null? (testInputRule2 input))) (rule2 input name))
        ((testInputRule3 input) (rule3 input name))
        ((testInputRule4 input) (rule4 input name))
        ((testInputRule5 input) (rule5 input name))
        ((testInputRule6 input) (rule6 input name))
        ((testInputRule7 input) (rule7 input name))
        ((testInputRule8 input) (rule8 input name))
        (else (output (pick-random generic-response))))) ; rule 11 has been implemented for you

;;; rule 1 functions
(define (testInputRule1 input)
  (cond ((eqv? 'do (car input)) #t)
        ((eqv? 'can (car input)) #t)
        ((eqv? 'will (car input)) #t)
        ((eqv? 'would (car input)) #t)
        (else #f)))

(define (rule1 input name)
  (output (removeQM (list-ref
           (list (list 'yes 'i (car input))
                 (append (list 'no name 'i (car input) 'not) (changePronouns (cddr input))))
                 (random 2)))))

;;; rule 2 functions
;;; returns a list of special topics included in the input
(define (testInputRule2 input)
  (cond ((null? input) '())
        ((eqv? 'family (car input)) (cons (car input) (testInputRule2 (cdr input))))
        ((eqv? 'friend (car input)) (cons (car input) (testInputRule2 (cdr input))))
        ((eqv? 'friends (car input)) (cons (car input) (testInputRule2 (cdr input))))
        ((eqv? 'mom (car input)) (cons (car input) (testInputRule2 (cdr input))))
        ((eqv? 'dad (car input)) (cons (car input) (testInputRule2 (cdr input))))
        ((eqv? 'brother (car input)) (cons (car input) (testInputRule2 (cdr input))))
        ((eqv? 'sister (car input)) (cons (car input) (testInputRule2 (cdr input))))
        ((eqv? 'girlfriend (car input)) (cons (car input) (testInputRule2 (cdr input))))
        ((eqv? 'boyfriend (car input)) (cons (car input) (testInputRule2 (cdr input))))
        ((eqv? 'children (car input)) (cons (car input) (testInputRule2 (cdr input))))
        ((eqv? 'son (car input)) (cons (car input) (testInputRule2 (cdr input))))
        ((eqv? 'daughter (car input)) (cons (car input) (testInputRule2 (cdr input))))
        ((eqv? 'child (car input)) (cons (car input) (testInputRule2 (cdr input))))
        ((eqv? 'wife (car input)) (cons (car input) (testInputRule2 (cdr input))))
        ((eqv? 'husband (car input)) (cons (car input) (testInputRule2 (cdr input))))
        ((eqv? 'home (car input)) (cons (car input) (testInputRule2 (cdr input))))
        ((eqv? 'dog (car input)) (cons (car input) (testInputRule2 (cdr input))))
        ((eqv? 'cat (car input)) (cons (car input) (testInputRule2 (cdr input))))
        ((eqv? 'pet (car input)) (cons (car input) (testInputRule2 (cdr input))))
        (else (testInputRule2 (cdr input)))))

(define (rule2 input name)
  (let ((topics (testInputRule2 input)))
        (output (list 'tell 'me 'more 'about 'your (pick-random topics) name))))

;;; rule 3 functions
(define (testInputRule3 input)
  (if (eqv? (car input) 'why)
      #t
      #f))

(define (rule3 input name)
  (output '(why not?)))

;;; rule 4 functions

(define (testInputRule4 input)
  (if (eqv? (car input) 'how)
      #t
      #f))

(define (rule4 input name)
  (output (pick-random (rule4responses))) #t)

(define (rule4responses) '((why do you ask?)
                           (how would an answer to that help you?)))

;;; rule 5 functions

(define (testInputRule5 input)
  (if (eqv? (car input) 'what)
      #t
      #f))

(define (rule5 input name)
  (output (pick-random (rule5responses))) #t)

(define (rule5responses) '((what do you think?)
                           (why do you ask?)))

;;; rule 6 functions

(define (testInputRule6 input)
  (cond ((null? input) #f)
        ((null? (cdr input)) (let ((word (symbol->string (car input))))
                               (if (string=? (string (string-ref word (- (string-length word) 1))) "?")
                                   #t
                                   #f)))
        (else (testInputRule6 (cdr input)))))

(define (rule6 input name)
  (output (pick-random (rule6responses))) #t)

(define (rule6responses) '((i don't know)
                           (i have no idea)
                           (i have no clue)
                           (maybe)))

;;; rule 7 functions
(define (testInputRule7 input)
  (cond ((null? input) #f)
        ((eqv? 'because (car input)) #t)
        (else (testInputRule7 (cdr input)))))

(define (rule7 input name)
  (output '(is that the real reason?)))

;;; rule 8 functions
(define (testInputRule8 input)
  (if (and (eqv? (car input) 'i)
           (cond ((eqv? (car (cdr input)) 'need) #t)
                 ((eqv? (car (cdr input)) 'think) #t)
                 ((eqv? (car (cdr input)) 'have) #t)
                 ((eqv? (car (cdr input)) 'want) #t)
                 (else #f)))
      #t
      #f))

(define (rule8 input name)
  (output (addQM (append (list 'why 'do 'you (car (cdr input))) (changePronouns (cddr input))))))

(define (addQM input)
  (cond ((null? input) '())
        ((null? (cdr input)) (string->symbol (string-append (symbol->string (car input)) "?")))
        (else (cons (car input) (addQM (cdr input))))))

;;; pick one random element from the list choices
(define (pick-random choices)
  (list-ref choices (random (length choices))))

;;; generic responses for rule 11
(define generic-response '((that\'s nice)
                           (good to know)
                           (can you elaborate on that?)))

;;; change pronouns
(define (changePronouns input)
  (cond ((null? input) '())
        ((eqv? 'i (car input)) (cons 'you (changePronouns (cdr input))))
        ((eqv? 'am (car input)) (cons 'are (changePronouns (cdr input))))
        ((eqv? 'my (car input)) (cons 'your (changePronouns (cdr input))))
        ((eqv? 'your (car input)) (cons 'my (changePronouns (cdr input))))
        ((eqv? 'me (car input)) (cons 'you (changePronouns (cdr input))))
        ((eqv? 'you (car input)) (cons 'me (changePronouns (cdr input))))
        (else (cons (car input) (changePronouns (cdr input))))))

;;; remove ? from input
;;; converts to a string, checks for a ?, and returns a symbol of a substring without the ?
(define (removeQM input)
  (cond ((null? input) '())
        ((null? (cdr input)) (let ((word (symbol->string (car input))))
                               (if (string=? (string (string-ref word (- (string-length word) 1))) "?")
                                   (list (string->symbol (substring word 0 (- (string-length word) 1))))
                                   (list (string->symbol word)))))
        (else (cons (car input) (removeQM (cdr input))))))

;;; for tests
(chat-with 'Derek)
