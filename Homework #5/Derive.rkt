#lang racket

(define (symderiv qf) `(λ ,(cadr qf) ,(symderivat(caadr qf) (caddr qf))))

(define (symderivat v e)
  (cond((symbol? e) (if(eq? v e) 1 0))
       ((not(pair? e)) 0)
       ((not(pair? (cdr e))) 0)
       (else (let((f (cadr e)))
               (case (car e)
                 ((+ -) (cons(car e) (map(λ(x) (symderivat v x))(cdr e))))
                 ((*) (cons '+ (map(λ(x) `(* ,(symderivat v x) ,@(remv x(cdr e))))(cdr e))))
                 ((/) (if(pair?(cddr e))
                         (symderivat v `(* ,f(/(* ,@(cddr e)))))
                         `(/ ,(symderivat v f) -1 (expt ,f 2))))
                 ((exp) `(* ,e ,(symderivat v f)))
                 ((log) `(/ ,(symderivat v f) ,f))
                 ((expt) `(* ,e ,(symderivat v `(* ,(caddr e) (log f)))))
                 ((sin) `(*(cos ,f) ,(symderivat v f)))
                 ((cos) `(* -1 (sin ,f) ,(symderivat v f)))
                 ((tan) `(/ ,(symderivat v f)(expt(cos ,f) 2)))
                 ((asin) `(/ ,(symderivat v (cadr e)) (expt(- 1(expt ,f 2))1/2)))
                 ((acos) `(/(- ,(symderivat v f))(expt(- 1(expt ,f 2))1/2)))
                 ((atan) (if (null?(cddr e))
                            `(/ ,(symderivat v f)(+ 1(expt ,f 2)))
                            (symderivat v `(atan(/ ,f ,(caddr e))))))
                 ((abs) `(*(/ ,f ,e),(symderivat v f)))
                 (else 'ERROR))))))

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

(define y 43)

(define (derivative f) (λ(x)(/(-(f(+ x dx))(f x))dx)))

(define dx 1/100000)

(define-namespace-anchor anchor)

(define env (namespace-anchor->namespace anchor))

(map(λ(qf) (list((derivative (eval qf env))1)
               ;((eval(symderiv qf)env)1)
               (symderiv qf)
               ))
    qfs)

    
(provide (all-defined-out))