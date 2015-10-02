;Elise McEllhiney
;Homework #2
;10/2/2014

#lang racket

(define-namespace-anchor a)
(define env (namespace-anchor->namespace a))

;function that runs the whole program
(define (derivatives qf)
  (append-map(λ(qf)
     (write qf)          
     (list
         (def  (eval qf env) 1)             ;definition evaluation at 1
         ((eval (symderiv qf) env) 1)       ;symbolic evaluation at 1
         (symderiv qf)                      ;symbolic function at x
         '***
         )) qfs))


;derivative by definition
(define (def function value) 
  (/ (- (function (+ value dx)) (function value)) dx))

(define dx 1/1000)

;define cube function to use for testing
(define (cube x) (* x x x))

;gets the symbolic derivative
(define (symderiv sf)
  (list 'λ (cadr sf)(symderiveat (caddr sf)(caadr sf))))

(define (symderiveat function variable)
  (cond
    ;outputs 0 if function is a number
    ((number? function) 0)
    ;checks if input function is only + - * or /
    ((and 
       (list? function)
       (= (length function) 1) 
       (or 
         (equal? (car function) '+)
         (equal? (car function) '*))) 0)
    ;checks if function is equal to variable as derivative of x dx is 1  
    ((equal? function variable) 1)
    ;checks if funciton is not a list and is not equal to variable
    ((and (not (list? function))(not (equal? function variable))) 0)
    
    (else(case(car function)
    
           ((+) 
               (add 
                    (symderiveat(cadr function) variable) 
                    (if(=(length function)3)
                       (symderiveat (caddr function) variable)
                       (symderiveat (cons '+ (cddr function)) variable))
                    ))
           
           ((-) 
               (subtract 
                     (symderiveat (cadr function) variable)
                     (cond
                            ((<(length function)3) 0)
                            ((=(length function)3) (symderiveat (caddr function) variable))
                            ((>(length function)3) (symderiveat (cons '+ (cddr function)) variable))
                            )))
           
           ((*) 
                (cond
                    ((= (length function) 1) 0)
                    ((= (length function) 2) (symderiveat (cadr function) variable))
                    (else
                       (+fix (list '+ 
                          (*fix (list '* (symderiveat(cadr function) variable) (append '(*) (cddr function))))
                          (*fix (list '* (cadr function) (symderiveat(append '(*) (cddr function)) variable))))
                 ))))
           
           ;(/ a b c d) = a/(b*c*d)
           ((/) 
              (cond
                  ((=(length function)1) 0)
                  ((=(length function)2) (quotient_rule 1 (cadr function) variable))
                  ((>(length function)2)(quotient_rule (cadr function) (*fix(append '(*) (cddr function))) variable))
                  ))
           
           ((exp) 
              (*fix(list '* (symderiveat (cadr function) variable) function)))
           
           ((sin) 
              (*fix(list '* (symderiveat (cadr function) variable) (list 'cos (cadr function)))))
           
           ((cos) 
              (*fix(list '* (symderiveat (cadr function) variable) (list '- (list 'sin (cadr function))))))
           
           ((tan) 
              (*fix(list '* (symderiveat (cadr function) variable) (list '/ 1 (*fix(list '* (list 'cos (cadr function))(list 'cos (cadr function))))))))
           
           ((asin)  
              (*fix(list '* (symderiveat (cadr function) variable) (list '/ 1 (list 'sqrt (list '- 1 (*fix(list '* (cadr function)(cadr function)))))))))
           
           ((acos) 
              (list '- (*fix(list '* (symderiveat (cadr function) variable) (list '/ 1 (list 'sqrt (list '- 1 (*fix(list '* (cadr function)(cadr function))))))))))
           
           ((atan)  
              (*fix(list '* (symderiveat (cadr function) variable) (list '/ 1 (list '+ 1 (*fix(list '* (cadr function)(cadr function))))))))
         
           ((expt)         
              (cond
                 ((and (number? (cadr function))(number? (caddr function))) 0)
                 
                 ;((and (member variable (cadr function))(not (member variable (caddr function))))
                    ;(list '* (caddr function) (list 'expt (cadr function) (- (caddr function) 1))))
                 
                 ((number? (cadr function)) (*fix(list '* (symderiveat (caddr function) variable) function (list 'log (cadr function)))))
                 (else
                   (list '* function 
                         (list '+ 
                               (list '* (symderiveat (cadr function) variable)(list '/ (caddr function) (cadr function)))
                               (list '* (symderiveat (caddr function) variable)(list 'log (cadr function))))))
                 ))

           ((abs) (*fix(list '* (symderiveat (cadr function) variable) (list '/ (cadr function) function))))
           
           ((log) (list '/ (symderiveat (cadr function) variable) (cadr function)))
           ))
))

(define (quotient_rule num den variable) 
   (list '/ 
         (list '- (*fix(list '* den (symderiveat num variable))) (*fix(list '* (symderiveat den variable) num)))
         (*fix(list '* den den))))

;This function subtracts
(define (subtract f s)
  (cond ((and (number? f)(number? s))(- f s))
        (else (list '- f s))))

;This function makes a product
(define (multiply f s)
  (cond ((or (equal? f 0)(equal? s 0)) 0)
        ((and (number? f)(number? s))(* f s))
        (else (list'*  f s))))

(define (*fix function)
  (cond
    ((member 0 function)0)
    ((member 1 function)(*fix (filter (λ(v) (not (equal? v 1))) (if (= (length function) 2) (cddr function) function)))) ;check that this works
    ((=(length function)1) 1)
    ((=(length function)2) (cadr function))
    ((and (=(length function)3)(number? (cadr function))(number? (cddr function)))(* (cadr function)(cddr function)))
    (else function)
  ))

;This function makes a exponential
(define (exponential f s)
  (cond ((and(number? f)(number? s)) (expt f s))
        (else (list 'expt f s))))

;This function makes a summation
(define (add f s)
  (cond ((equal? f 0) s)
        ((equal? s 0) f)
        ((and (number? f)(number? s))(+ f s))
        (else (list '+ f s))))

(define (+fix function)
  (cond
    ((member 0 function)(+fix (filter (λ(v) (not (equal? v 0))) function)))
    ((=(length function)1) 0)
    ((=(length function)2) (cadr function))
    ((and (=(length function)3)(number? (cadr function))(number? (cddr function)))(+ (cadr function)(cddr function)))
    (else function)
  ))


(define y 43)

;list of qfs given in class
(define num        '(λ(x) 47))
(define numc       '(λ(x) 2+3i))
(define xbound     '(λ(x) x))
(define yfree      '(λ(x) y));note y is free in y free so give it a value by defining y to be 43 done above
(define x+         '(λ(x) (+ x 3 x 5 x)))
(define x+nil      '(λ(x) (+)))
(define x-         '(λ(x) (- x 3 x 5 x)))
(define x1-        '(λ(x) (- x))) ;wrogjnd
(define x*         '(λ(x) (* x 3 x 5 x)))
(define x*nil      '(λ(x) (*)))
(define x/         '(λ(x) (/ x 3 x 5 x)))
(define x1/        '(λ(x) (/ x)))
(define xexp       '(λ(x) (exp x)))
(define xlog       '(λ(x) (log x)))
(define xexplog    '(λ(x) (exp (log x))))
(define xlogexp    '(λ(x) (log (exp x))))
(define xexpt      '(λ(x) (expt x 3)))
(define xexpt2     '(λ(x) (expt 2 x)))
(define xexpt3     '(λ(x) (expt x x)))
(define xexpi      '(λ(x) (exp(* +i x))))
(define xexpii     '(λ(x) (exp (* (expt -1 1/2) x))))
(define xcos       '(λ(x) (+ (cos x)(* (expt -1 1/2) (sin x)))))
(define xsin       '(λ(x) (log (sin x))))
(define xtan       '(λ(x) (tan x)))
(define xasin      '(λ(x) (asin (- x 1/2))))
(define xacos      '(λ(x) (acos (- x 1/2))))
(define xatan      '(λ(x) (atan x)))
(define xabs       '(λ(x) (abs (- x 2))))
(define archimedes '(λ(lat) (sin lat)))
(define mercator   '(λ(lat) (log (+ (tan lat)(/ (cos lat))))))

;list of qfs
(define qfs (list 
                  num numc xbound yfree 
                  x+ x+nil x- x1- 
                  x* x*nil x/ x1/
                  xexp xlog xexplog xlogexp
                  xexpt xexpt2 xexpt3 xexpi xexpii
                  xcos xsin xtan xasin xacos 
                  xatan xabs
                  archimedes mercator))

;runs the program for grading purposes
(derivatives qfs)
;(symderiv xatan)
;;;; ((eval (symderiv '(λ (x) (/ 1 (＋ 1 (* x x))))) env) 1) 
;;;; ((eval (symderiv '(λ (x) (/ 1 (＋ 1 (* x x))))) env) 1) 
;((eval '(lambda(x) (+)) env) 5)
 ;((eval (symderiv xatan) env) 1) 