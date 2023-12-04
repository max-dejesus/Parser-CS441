#lang racket

;; Parser Project
;; Max DeJesus
;; All of my used resources consist of the Racket documentation

(define (scanfile str)
  (file->lines str))

(define (last lst) 
  (first (reverse lst)))

(define (operator? str)
  (cond 
    [(equal? str "(") 'lparen]
    [(equal? str ")") 'rparen]
    [(equal? str "+") 'plus] 
    [(equal? str "-") 'minus] 
    [(equal? str "=") 'equals]
    [(equal? str ":") 'colon] 
    [else #f]))

(define (keyword? str)
  (cond
    [(equal? str "goto") 'goto]
    [(equal? str "read") 'read]
    [(equal? str "write") 'write]
    [(equal? str "gosub") 'gosub]
    [(equal? str "if") 'if]
    [(equal? str "then") 'then]
    [(equal? str "return") 'return]
    [else #f]
    ))

(define (id? str)
  (let ([frst (string-ref str 0)])
    (cond
      [(not (char-alphabetic? frst)) #f]
      [(string? str) 'id]
      )
    )
  )

; turns string into its respective token and returns
(define (tokenize str)
  (cond
    [(equal? str "$$") '$$]
    [(number? (string->number str))
     (begin
       (if (> (string->number str) 0) 'idx
           'num))]
    [(keyword? str)]
    [(id? str)]
    [(operator? str)]
    [else 'err]
    )
  )

(define (stmt? t la) ; determine grammar rules for stmts
  (cond
    [(equal? t 'read) (if (equal? la 'id) #t #f)]
    [(equal? t 'goto) (if (equal? la 'idx) #t #f)]
    [(equal? t 'gosub) (if (equal? la 'idx) #t #f)]
    [(equal? t 'return) (if (equal? la '()) #t #f)]
    [(equal? t 'id) (if (equal? la 'equals) #t #f)]
    [(member t (list 'then 'colon))
     (cond
       [(member la (list 'id 'if 'read 'write 'goto 'gosub 'return)) #t]
       [else #f]
       )]
    [(member t (list 'if 'write))
     (cond
       [(member la (list 'id 'idx 'num 'lparen)) #t]
       [else #f]
       )]
    [else #f]
    )
  )

(define (expr? t la) ; determine grammar rules for exprs
  (cond
    [(member t (list 'idx 'id 'num))
     (cond
       [(equal? la 'plus) #t]
       [(equal? la 'minus) #t]
       [(equal? la 'equals) #t]
       [(equal? la 'then) #t]
       [(equal? la 'rparen) #t]
       [(equal? la '()) #t]
       [else #f]
       )]
    [(member t (list 'plus 'minus 'equals))
     (cond
       [(equal? la 'id) #t]
       [(equal? la 'idx) #t]
       [(equal? la 'num) #t]
       [(equal? la 'lparen) #t]
       [else #f]
       )]
    [(equal? t 'lparen)
     (cond
       [(member la (list 'id 'idx 'num)) #t]
       [else #f]
       )]
    [(equal? t 'rparen)
     (cond
       [(member la (list 'plus 'minus 'equals '() 'then)) #t]
       [else #f]
       )]  
    [else #f]
    )
  )


(define (parse filename)
  (define linelist (scanfile filename)) ; list of LINES in program
  (with-handlers ([exn:fail:syntax? (lambda (e) ((error-display-handler) (exn-message e) e))])
    (if (equal? (last linelist) "$$")
        #t
        (raise-syntax-error 'SyntaxError "no end of input"))
    
    (for ([e (in-list linelist)]) ; for loop over each LINE in linelist, e = one line
      (letrec
          ([syntaxlist (string-split e)] ; split current line into list of syntax
           [lineno (list-ref syntaxlist 0)] 
           [tokenlist (map tokenize syntaxlist)] ; tokenized syntaxlist
           [parsestack '()])

        (for ([t (in-list tokenlist)]
              [i (in-range 1 (+ (length tokenlist) 1))])
          (define la null)
          (cond
            [(< i (length tokenlist))
             (cond
               [(equal? (list-ref tokenlist i) 'colon) (set! la '())]
               [else (set! la (list-ref tokenlist i))]
               )]
            [else (set! la '())]
            )

          (if (equal? i 1)
              (cond
                [(equal? t '$$)]
                [(equal? t 'idx)]
                [else (raise-syntax-error 'SyntaxError "no line number")]
                ) 
              (begin
                (if (equal? t 'err)
                    (raise-syntax-error 'SyntaxError "invalid grammar")
                    #f)

                (if (empty? parsestack)
                    (if (stmt? t la)
                        (set! parsestack (list (append parsestack t)))
                        (raise-syntax-error 'SyntaxError (string-append "Error on line " lineno)))
                    (cond
                      [(expr? t la) (set! parsestack (list (append parsestack t)))]
                      [(stmt? t la) (set! parsestack (list (append parsestack t)))]
                      [else (raise-syntax-error 'SyntaxError (string-append "Error on line " lineno))]
                      )
                    ) ; if empty
                ) ;begin
              ) ; if first item
          ) ; for t in token     
        ) ;letrec 
      ) ; for e in linelist

    (displayln "Accepted") ; accepts if full function is ran w/ no errors
    
    ) ; exn handler
  ) ; function

(parse "file01.txt")
(parse "file02.txt")
(parse "file03.txt")
(parse "file04.txt")
(parse "file05.txt")
(parse "file06.txt")
