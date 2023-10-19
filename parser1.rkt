#lang racket

; Define the Either monad
(define (Left x) (cons 'Left x))
(define (Right x) (cons 'Right x))
(define (is-Left x) (eq? (car x) 'Left))
(define (is-Right x) (eq? (car x) 'Right))
(define (from-Left x) (cdr x))
(define (from-Right x) (cdr x))

(define (string-index str char)
  (let ([match (regexp-match-positions (regexp-quote char) str)])
    (if match (caar match) #f)))

    (define (butlast lst)
  (reverse (cdr (reverse lst))))


; Define a helper function to match regex patterns
(define (match-regex pattern str)
  (if (regexp-match pattern str) (Right str) (Left "Syntax error")))

; Define parsing functions for non-terminals
(define (parse-id str)
  (match-regex #rx"^[a-zA-Z][a-zA-Z0-9]*" str))

(define (parse-num str)
  (match-regex #rx"^[+-]?[0-9]+" str))

; ... other non-terminal parsing functions ...
; ... previous code ...

; Define helper function to check if a string starts with a given prefix
(define (starts-with? str prefix)
  (string-prefix? str prefix))

; Define helper function to remove a prefix from a string
(define (remove-prefix str prefix)
  (substring str (string-length prefix)))

; ... previous code ...

; Define a helper function to check if a string is a reserved word
(define (is-reserved-word? str)
  (member str '("if" "while" "goto" "gosub" "return" "break" "end")))

; Define parsing functions for non-terminals
; ... previous code ...

(define (parse-etail str)
  (cond
    [(starts-with? str "+") (parse-expr (remove-prefix str "+"))]
    [(starts-with? str "-") (parse-expr (remove-prefix str "-"))]
    [(starts-with? str "*") (parse-expr (remove-prefix str "*"))]
    [(starts-with? str "/") (parse-expr (remove-prefix str "/"))]
    [else (Right str)])) ; epsilon case, no operation to perform

(define (parse-expr str)
  (cond
    ; Check for id
    [(is-Right (parse-id str))
     (let ([id-end (or (string-index str " ") (string-length str))])
       (parse-etail (substring str id-end)))]
    ; Check for num
    [(is-Right (parse-num str))
     (let ([num-end (or (string-index str " ") (string-length str))])
       (parse-etail (substring str num-end)))]
    ; Check for (expr)
    [(and (starts-with? str "(") (string-suffix? str ")"))
     (parse-expr (substring str 1 (- (string-length str) 1)))]
    [else (Left "Syntax error in expr")])) 

; ... rest of the code ...


(define (parse-boolean str)
  (cond
    [(starts-with? str "true") (Right (remove-prefix str "true"))]
    [(starts-with? str "false") (Right (remove-prefix str "false"))]
    [else
     (let ([expr-result (parse-expr str)])
       (if (is-Right expr-result)
           ; Handle bool-op and the second expr here
           ; For simplicity, let's assume a basic case with '='
           (if (starts-with? (from-Right expr-result) "=")
               (parse-expr (remove-prefix (from-Right expr-result) "="))
               (Left "Syntax error in boolean"))
           (Left "Syntax error in boolean")))]))

(define (parse-stmt str)
  (cond
    [(starts-with? str "if (")
     ; Handle if statement
     (let* ([bool-end (string-index str ")")]
            [bool-str (substring str 4 bool-end)]
            [bool-result (parse-boolean bool-str)])
       (if (is-Right bool-result)
           ; Continue parsing the stmt after the boolean
           (parse-stmt (substring str (+ bool-end 1)))
           (Left "Syntax error in if statement")))]
    [(starts-with? str "while (")
     ; Handle while statement
     ; Similar approach as the if statement
     (Right (remove-prefix str "while ("))]
    [(starts-with? str "id =")
     ; Handle assignment statement
     (if (not (is-reserved-word? (substring str 0 (string-index str " "))))
         (parse-expr (remove-prefix str "id ="))
         (Left "Syntax error: Reserved word used as label"))]
    ; ... handle other statements ...
    [(starts-with? str "read ")
     (if (is-Right (parse-id (remove-prefix str "read ")))
         (Right (remove-prefix str "read id;"))
         (Left "Syntax error in read statement"))]
    [(starts-with? str "write ")
     (if (is-Right (parse-expr (remove-prefix str "write ")))
         (Right (remove-prefix str "write expr;"))
         (Left "Syntax error in write statement"))]
    [(starts-with? str "goto ")
     (if (is-Right (parse-id (remove-prefix str "goto ")))
         (Right (remove-prefix str "goto id;"))
         (Left "Syntax error in goto statement"))]
    [(starts-with? str "gosub ")
     (if (is-Right (parse-id (remove-prefix str "gosub ")))
         (Right (remove-prefix str "gosub id;"))
         (Left "Syntax error in gosub statement"))]
    [(starts-with? str "return")
     (Right (remove-prefix str "return;"))]
    [(starts-with? str "break")
     (Right (remove-prefix str "break;"))]
    [(starts-with? str "end")
     (Right (remove-prefix str "end;"))]
    [else (Left "Syntax error in stmt")]))
    
; ... rest of the code ...


(define (parse-line str)
  (if (starts-with? str "id:")
      ; ... handle label ...
      (Right (remove-prefix str "id:"))
      (parse-stmt str)))

(define (parse-linelist lines line-number)
  (if (null? lines)
      (Right '())
      (let ([result (parse-line (car lines))])
        (if (is-Right result)
            (parse-linelist (cdr lines) (+ line-number 1))
            (Left (format "Syntax error on line ~a" line-number))))))

(define (parse-program lines)
  (if (and (not (null? lines)) (string=? (car (reverse lines)) "$$"))
      (parse-linelist (butlast lines) 1) ; Start with line number 1
      (Left "Program does not end with $$")))


; ... rest of the code ...



(define (parse filename)
  (let ([lines (file->lines filename)])
    (if (is-Right (parse-program lines))
        "Accept"
        (from-Left (parse-program lines)))))

; Test
(display (parse "source1.txt"))
