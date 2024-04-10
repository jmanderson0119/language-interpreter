;;;; ************************
;;;; Jacob Anderson (jma195), Hayden Caldwell (hwc11), David Kraus (dek90)
;;;; CSDS 345 Spring 2024
;;;; Flow Control Interpreter Project
;;;; ************************

#lang racket

;; add parser to interpreter
(require "simpleParser.rkt")


;;;; STATE MANAGEMENT ABSTRACTION

;; abstraction for the current variable in the state recursion
(define curr-variable (lambda (state) (caaar state)))

;; abstraction for the value of the current variable in the state recursion
(define curr-val (lambda (state) (cadr (car (car state)))))

;; abstraction for the current variable in the layer recursion
(define layer-curr-variable (lambda (state) (caar state)))

;; abstraction for the value of the current variable in the layer recursion
(define layer-curr-val (lambda (state) (cadr (car state))))


;;;; ERROR MESSAGES

;; error message for when a variable is used before it is declared
(define error-message (lambda () (error "variable has not been declared")))

;; error message for when a binding is created for a variable already in the state
(define create-binding-error-message (lambda () (error "variable has already been declared")))


;;;; STATE MANAGEMENT FUNCTIONS

;; creates a brand new state containing one state layer
(define create-new-state (lambda () '(())))

;; adds a new layer to state
(define add-state-layer (lambda (state) (cons '() state)))

;; removes the top layer of state
(define remove-state-layer (lambda (state) (cdr state)))

;; confirm the existence of a variable in the specified state
(define contains
    (lambda (var state)
        (cond
            [(null? state)                                      #f]
            [(null? (car state))                                (contains var (cdr state))]
            [(and (list? (car state)) (eq? var (curr-variable state)))  #t]
            [else                                               (contains var (cons (cdr (car state)) (cdr state)))]
        )))

;; confirm the existence of a variable in a specified layer
(define layer-contains
    (lambda (var layer)
        (cond 
            [(null? layer)                          #f]
            [(eq? var (layer-curr-variable layer))  #t]
            [else                                   (layer-contains var (cdr layer))]
        )))

;; lookup a variable's binding and return value assigned to it
(define lookup
    (lambda (var state)
        (cond
            [(not (contains var state))         (error-message)]
            [(null? (car state))                (lookup var (cdr state))]
            [(eq? var (curr-variable state))    (curr-val state)]
            [else                               (lookup var (cons (cdr (car state)) (cdr state)))]
        )))

;; lookup a value from a known binding in a layer
(define layer-lookup
    (lambda (var layer)
        (cond 
            [(not (layer-contains var layer))       (error-message)]
            [(eq? var (layer-curr-variable layer))  (layer-curr-val layer)]
            [else                                   (layer-contains var (cdr layer))]
        )))

;; add a new binding to the state in the current scope
(define create-binding
    (lambda (var val state)
        (cond
            [(contains var state)   (create-binding-error-message)]
            [else                   (cons (cons (cons var (cons val '())) (car state)) (cdr state))]
        )))

;; add a new binding to the state
(define layer-create-binding
    (lambda (var val layer)
        (cond 
            [(not (layer-contains var layer))   (cons (list var val) layer)]
            [else                               (create-binding-error-message)]
        )))

;; update a binding currently in the state
(define update-binding
    (lambda (var val state)
        (cond
            [(not (contains var state))                                                         (error-message)]
            [(and (not (layer-contains var (car state))) (layer-contains var (cadr state)))     (cons (car state) (update-binding var val (cdr state)))]
            [(eq? var (curr-variable state))                                                    (cons (cons (cons var (cons val '())) (cdr (car state))) (cdr state))]
            [else                                                                               (cons (cons (caar state) (layer-update-binding var val (cdr (car state)))) (cdr state))]
        )))

;; update a binding currently in the specified layer
(define layer-update-binding
    (lambda (var val layer)
        (cond 
            [(null? layer)                (error-message)]
            [(layer-contains var layer)   (layer-create-binding var val (layer-delete-binding var layer))]
            [else                         (layer-create-binding var val layer)]
        )))

;; delete a binding currently in the specified layer
(define layer-delete-binding
    (lambda (var layer)
        (cond
            [(or (null? layer) (not (layer-contains var layer)))    layer]
            [(eq? var (layer-curr-variable layer))                  (cdr layer)]
            [else                                                   (cons (car layer) (layer-delete-binding var (cdr layer)))]
        )))


;;;; VALUE FUNCTION ABSTRACTION

;; abstraction for type of expression
(define expr-type (lambda (expr) 
    (if (list? expr)
        (car expr)
        expr)))

;; abstraction for first operand
(define firstoperand 
    (lambda (expr state) 
        (if (or (number? (cadr expr)) (boolean? (cadr expr)))
            (list (cadr expr))
            (eval-expression (cadr expr) state))))

;; abstraction for second operand
(define secondoperand
    (lambda (expr state) 
        (if (or (number? (caddr expr)) (boolean? (caddr expr)))
            (list (caddr expr))
            (eval-expression (caddr expr) state))))

;; abstraction for unary condition
(define unary? (lambda (expr) (eq? (length expr) 2)))


;;;; VALUE PROCEDURES

;; Evaluates an expression
(define eval-expression
    (lambda (expr state)
        (cond
            ((or (number? (expr-type expr)) (boolean? (expr-type expr))) (expr-type expr))
            ((and (list? expr) (unary? expr) (eq? (expr-type expr) '-)) (- (eval-expression (firstoperand expr state) state)))
            ((and (list? expr) (unary? expr) (eq? (expr-type expr) '!)) (if (eval-expression (firstoperand expr state) state) #t #f))
            ((and (list? expr) (eq? (expr-type expr) '+)) (+ (eval-expression (firstoperand expr state) state) (eval-expression (secondoperand expr state) state)))
            ((and (list? expr) (eq? (expr-type expr) '-)) (- (eval-expression (firstoperand expr state) state) (eval-expression (secondoperand expr state) state)))
            ((and (list? expr) (eq? (expr-type expr) '*)) (* (eval-expression (firstoperand expr state) state) (eval-expression (secondoperand expr state) state)))
            ((and (list? expr) (eq? (expr-type expr) '/)) (quotient (eval-expression (firstoperand expr state) state) (eval-expression (secondoperand expr state) state)))
            ((and (list? expr) (eq? (expr-type expr) '%)) (remainder (eval-expression (firstoperand expr state) state) (eval-expression (secondoperand expr state) state)))
            ((and (list? expr) (eq? (expr-type expr) '==)) (eq? (eval-expression (firstoperand expr state) state) (eval-expression (secondoperand expr state) state)))
            ((and (list? expr) (eq? (expr-type expr) '!=)) (not (eq? (eval-expression (firstoperand expr state) state) (eval-expression (secondoperand expr state) state))))
            ((and (list? expr) (eq? (expr-type expr) '<)) (< (eval-expression (firstoperand expr state) state) (eval-expression (secondoperand expr state) state)))
            ((and (list? expr) (eq? (expr-type expr) '>)) (> (eval-expression (firstoperand expr state) state) (eval-expression (secondoperand expr state) state)))
            ((and (list? expr) (eq? (expr-type expr) '<=)) (<= (eval-expression (firstoperand expr state) state) (eval-expression (secondoperand expr state) state)))
            ((and (list? expr) (eq? (expr-type expr) '>=)) (>= (eval-expression (firstoperand expr state) state) (eval-expression (secondoperand expr state) state)))
            ((and (list? expr) (eq? (expr-type expr) '||)) (or (eval-expression (firstoperand expr state) state) (eval-expression (secondoperand expr state) state)))
            ((and (list? expr) (eq? (expr-type expr) '&&)) (and (eval-expression (firstoperand expr state) state) (eval-expression (secondoperand expr state) state)))
            ((symbol? (expr-type expr)) (lookup (expr-type expr) state)))))


;;;; STATE FUNCTION ABSTRACTIONS

;; abstraction for return value
(define returnval
  (lambda (stmt)
    (cond
    ((list? (cadr stmt)) (cadr stmt))
    (else (list (cadr stmt))))))

;; abstraction for conditional-statement condition
(define condition (lambda (stmt) (cadr stmt)))

;; abstraction for conditional-statement then-statement
(define then-statement (lambda (stmt) (caddr stmt)))

;; abstraction for conditional-statement else-statement
(define else-statment (lambda (stmt) (cadddr stmt)))

;; abstraction for no optional-else-statement condition
(define no-else-statement? (lambda (stmt) (eq? (length stmt) 3)))

;; abstraction for the variable name
(define varname (lambda (stmt) (cadr stmt)))

;; abstraction for the variable value
(define varval 
    (lambda (stmt state)
        (cond
            ((or (number? (caddr stmt)) (boolean? (caddr stmt)) (symbol? (caddr stmt))) (list (caddr stmt)))
            (eval-expression (caddr stmt) state))))

;; abstraction for type of statement
(define stmt-type (lambda (stmt) (car stmt)))

;; abstraction for body of while statement
(define while-body (lambda (stmt) (caddr stmt)))

;; abstraction for declaration-only condition
(define declaration-only? (lambda (stmt) (eq? (length stmt) 2)))

;; abstraction for try body
(define try-body (lambda (stmt) (caddr stmt)))

;; abstraction for finally body
(define finally-body (lambda (stmt) (caddr stmt)))


;;;; STATE FUNCTIONS

;; evaluates return statements
(define eval-return
    (lambda (stmt state)
        (eval-expression (returnval stmt) state)))

;; evaluates if statements without else-statement
(define eval-if-then
    (lambda (stmt state next break continue throw)
        (if (eval-expression (condition stmt) state)
            (eval-statement (then-statement stmt) state next break continue throw)
            (next state))))

;; evaluates if statements with else-statement
(define eval-if-then-else
    (lambda (stmt state next break continue throw)
        (if (eval-expression (condition stmt) state)
            (eval-statement (then-statement stmt) state next break continue throw )
            (eval-statement (else-statment stmt) state next break continue throw))))

;; evaluates while statements
(define eval-while
    (lambda (stmt state next break continue throw)
        (if (eval-expression (condition stmt) state)
            (eval-while stmt (eval-statement (while-body stmt) state (next (lambda (s) (eval-while stmt s next break continue throw))) break continue throw) )
            (next state))))

;; evaluates try-catch-finally blocks
(define eval-try
  (lambda (stmt state next break continue throw)
    (eval-statement (try-body stmt) state next break continue throw)
    (cond
      ((not (state)) break))
    (eval-statement (finally-body stmt) state next break continue throw)))


;; STATE PROCEDURES

;; selects the appropriate evaluation procedure for a given statement
(define eval-statement
    (lambda (stmt state next oldbreak oldContinue oldThrow)
        (cond
            ((and (eq? (stmt-type stmt) 'var) (declaration-only? stmt)) (create-binding (varname stmt) 'null state))
            ((eq? (stmt-type stmt) 'var) (create-binding (varname stmt) (eval-expression (varval stmt state) state) state))
            ((eq? (stmt-type stmt) '=) (update-binding (varname stmt) (eval-expression (varval stmt state) state) state))
            ((eq? (stmt-type stmt) 'return) (eval-return stmt state))
            ((and (eq? (stmt-type stmt) 'if) (no-else-statement? stmt)) (eval-if-then stmt state next oldbreak oldContinue oldThrow))
            ((eq? (stmt-type stmt) 'if) (eval-if-then-else stmt state next oldbreak oldContinue oldThrow))
            ((eq? (stmt-type stmt) 'while) (eval-while stmt state next (oldbreak (lambda (s) (next s))) (oldContinue (lambda (s) (eval-while stmt s next (oldbreak (lambda (s) (next s))) oldContinue oldThrow))) (oldThrow (lambda (error) (oldThrow error)))))
            ((eq? (stmt-type stmt) 'try) (eval-try stmt state next oldbreak oldContinue oldThrow)))))


;;;; INTERPRET

;; interpret calls 'parser' and returns the syntax tree to an evaluation function
(define interpret
    (lambda (filename)
        (eval-syntaxtree (parser filename) (create-new-state))))

;; eval-syntaxtree will continue through 
(define eval-syntaxtree
    (lambda (syntaxtree state)
        (cond
            ((null? syntaxtree) (void))
            ((eq? (stmt-type (car syntaxtree)) 'return) (eval-statement (car syntaxtree) state state (error "Break statement not within a loop") (error "Continue statement not within a loop") (error "Unhandled exception" error)))
            (else (eval-syntaxtree (cdr syntaxtree) (eval-statement (car syntaxtree) state state (error "Break statement not within a loop") (error "Continue statement not within a loop") (error "Unhandled exception" error)))))))

(interpret "test1.txt")
;test 1 works
;test 2 works
;test 3 works
;test 4 works
;test 5 
;   *: contract violation
;   expected: number?  
;   given: #<void>
;test 6 
;   <=: contract violation
;   expected: real?     
;   given: #<void>    
;test 7 
;   >=: contract violation
;   expected: real?     
;   given: #<void> 
;test 8 works
;test 9 
;   firstoperand: arity mismatch;
;   the expected number of arguments does not match the given number
;   expected: 2
;   given: 1
;test 10 works
;test 11
;   returns 11
;test 12 works
;test 13 
;   variable has not been declared
;test 14
;   +: contract violation
;   expected: number?  
;   given: #<void>  
;test 15 
;   variable has not been declared
;test 16 
;   contract violation
;   expected: real?    
;   given: #<void> 
;test 17 
;   firstoperand: arity mismatch;
;   the expected number of arguments does not match the given number
;   expected: 2
;   given: 1
;test 18 
;   returns #t not true
;test 19
;   contract violation        
;   expected: (cons/c pair? any/c)
;   given: #<void>
;test 20
;   *: contract violation
;   expected: number?  
;   given: #<void>
;
            