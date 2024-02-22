;;;; ************************
;;;; Jacob Anderson (jma195), Hayden Caldwell (abc123), David Kraus (abc123)
;;;; CSDS 345 Spring 2024
;;;; Simple Language Interpreter Project
;;;; ************************

#lang racket

;; add parser to interpreter
(require "simpleParser.rkt")


;;;; STATE MANAGEMENT ABSTRACTION

;; getter
(define get (lambda (state) state))

;; abstraction for the current variable in the state recursion
(define curr-variable (lambda (state) (caar state)))

;; abstraction for the value of the current variable in the state recursion
(define curr-val (lambda (state) (cadr (car state))))

;; abstraction for the rest of the state to recurse over
(define remaining-state (lambda (state) (cdr state)))


;;;; ERROR MESSAGES

;; error message for when a variable is used before it is declared
(define error-message (lambda () (error "variable has not been declared")))

;; error message for when a binding is created for a variable already in the state
(define create-binding-error-message (lambda () (error "variable has already been declared")))


;;;; STATE MANAGEMENT FUNCTIONS

;; search for a variable in the specified state
(define invoke
    (lambda (var state)
        (cond 
            ((null? state) #f)
            ((eq? var (curr-variable state)) #t)
            (else (invoke var (remaining-state state))))))

;; lookup a value from a known binding
(define lookup
    (lambda (var state)
        (cond 
            ((not (invoke var state)) (error-message))
            ((eq? var (curr-variable state)) (curr-val state))
            (invoke var (remaining-state state)))))

;; add a new binding to the state
(define create-binding
    (lambda (var val state)
        (cond 
            ((null? state) (cons (list var val) state))
            ((not (lookup var state)) (cons (list var val) state))
            (else (create-binding-error-message)))))

;; update a binding currently in the state
(define update-binding
    (lambda (var val state)
        (cond 
            ((null? state) (error-message))
            ((invoke var state) (create-binding var val (delete-binding var state)))
            (else (create-binding var val state)))))

;; delete a binding currently in the state
(define delete-binding
    (lambda (var state)
        (cond
            ((or (null? state) (not (invoke var state))) state)
            ((eq? var (curr-variable state)) (remaining-state state))
            (else (cons (car state) (delete-binding var (remaining-state state)))))))

;;;; VALUE FUNCTION ABSTRACTION

;; abstraction for type of expression
(define expr-type (lambda (expr)
                    (if (list? expr)
                    (car expr)
                    expr)))

;; abstraction for first operand
(define firstoperand 
    (lambda (expr state) 
        (cond
            ((or (number? (cadr expr)) (boolean? (cadr expr)) (symbol? (cadr expr))) (list (cadr expr)))
            (eval-expression (cadr expr) state))))

;; abstraction for second operand
(define secondoperand
    (lambda (expr state) 
        (cond
            ((or (number? (caddr expr)) (boolean? (caddr expr)) (symbol? (caddr expr))) (list (caddr expr)))
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
            ((symbol? (expr-type expr)) (invoke (expr-type expr) state)))))


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


;;;; STATE FUNCTIONS

;; evaluates return statements
(define eval-return
    (lambda (stmt state)
        (eval-expression (returnval stmt) state)))

;; evaluates if statements without else-statement
(define eval-if-then
    (lambda (stmt state)
        (if (eval-expression (condition stmt) state)
            (eval-statement (then-statement stmt) state)
            (void))))

;; evaluates if statements with else-statement
(define eval-if-then-else
    (lambda (stmt state)
        (if (eval-expression (condition stmt) state)
            (eval-statement (then-statement stmt) state)
            (eval-statement (else-statment stmt) state))))

;; evaluates while statements
(define eval-while
    (lambda (stmt state)
        (if (eval-expression (condition stmt) state)
            (eval-while stmt (eval-statement (while-body stmt) state))
            (void))))


;; STATE PROCEDURES

;; selects the appropriate evaluation procedure for a given statement
(define eval-statement
    (lambda (stmt state)
        (cond
            ((and (eq? (stmt-type stmt) 'var) (declaration-only? stmt)) (create-binding (varname stmt) 'null state))
            ((eq? (stmt-type stmt) 'var) (create-binding (varname stmt) (eval-expression (varval stmt state) state) state))
            ((eq? (stmt-type stmt) '=) (update-binding (varname stmt) (eval-expression (varval stmt state) state) state))
            ((eq? (stmt-type stmt) 'return) (eval-return stmt state))
            ((and (eq? (stmt-type stmt) 'if) (no-else-statement? stmt)) (eval-if-then stmt state))
            ((eq? (stmt-type stmt) 'if) (eval-if-then-else stmt state))
            ((eq? (stmt-type stmt) 'while) (eval-while stmt state)))))


;;;; INTERPRET

;; interpret calls 'parser' and returns the syntax tree to an evaluation function
(define interpret
    (lambda (filename)
        (eval-syntaxtree (parser filename) '())))

;; eval-syntaxtree will continue through 
(define eval-syntaxtree
    (lambda (syntaxtree state)
        (cond
            ((null? syntaxtree) (void))
            ((eq? (stmt-type (car syntaxtree)) 'return) (eval-statement (car syntaxtree) state))
            (else (eval-syntaxtree (cdr syntaxtree) (eval-statement (car syntaxtree) state))))))
        
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

