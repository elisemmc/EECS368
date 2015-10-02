;Elise McEllhiney
;Homework Assignment #3
;28 October 2014

#lang racket
(require racket/class)

(define make-account% 
  (class object%
    (super-new)
    (init-field (balance 0))
    (define/public (view) balance)
    (define/public (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount)) balance)
          "Insufficient funds"))
    (define/public (deposit amount)
      (set! balance (+ balance amount))
      balance)
    ))

(define make-accountl%
  (class make-account%
    (super-new)
    (inherit-field balance)
    (inherit view)
    (init-field (ledger (list (list 'start '= balance))))
    (define/override (withdraw amount)
      (if (eq? (super withdraw amount) "Insufficient funds")
          "Insufficient funds"
          (begin 
            (set! ledger (append ledger (list (list '- amount '= balance))))
            balance)))
    (define/override (deposit amount)
      (super deposit amount)
      (set! ledger (append ledger (list (list '+ amount '= balance))))
      balance)
    (define/public (show) ledger)
   ))

(define make-accountlp%
  (class make-accountl%
    (super-new)
    (init-field (password 'password))
    (inherit-field balance ledger)
    (define/override (withdraw pass amount)
      (if (equal? password pass) (super withdraw amount) "Incorrect password")
    )
    (define/override (deposit pass amount)
      (if (equal? password pass) (super deposit amount) "Incorrect password")
    )
    (define/override (view pass)
      (if (equal? password pass) (super view) "Incorrect password")
    )
    (define/override (show pass)
      (if (equal? password pass) (super show) "Incorrect password")
    )
  ))
  
(define make-accountlpa%
  (class make-accountlp%
    (super-new)
    (init-field (count 0))
    (inherit-field balance password ledger)
    (define/override (withdraw pass amount)
      (let ((result (super withdraw pass amount)))
      (if (equal? result "Incorrect password") 
          (begin 
            (set! count (+ count 1)) 
            (if (>= count 8) "cops called" result)) (begin (set! count 0) result)))  
    )
    (define/override (deposit pass amount)
      (let ((result (super deposit pass amount)))
      (if (equal? result "Incorrect password") 
          (begin 
            (set! count (+ count 1)) 
            (if (>= count 8) "cops called" result)) (begin (set! count 0) result)))
    )
    (define/override (view pass)
      (let ((result (super view pass)))
      (if (equal? result "Incorrect password") 
          (begin 
            (set! count (+ count 1)) 
            (if (>= count 8) "cops called" result)) (begin (set! count 0) result)))
    )
    (define/override (show pass)
      (let ((result (super show pass)))
      (if (equal? result "Incorrect password") 
          (begin 
            (set! count (+ count 1)) 
            (if (>= count 8) "cops called" result)) (begin (set! count 0) result)))
      
    )
  ))

(define make-accountlpat%
  (class make-accountlpa%
    (super-new)
    (inherit-field count balance password ledger)
    (inherit deposit withdraw view show)
    (define/public (transfer pass1 account pass2)
      (let ((amount (view pass1)) (check (send account view pass2)))
        (if (or (eq? amount "Incorrect password") (eq? amount "cops called") (eq? check "Incorrect password") (eq? check "cops called"))
            (if (or (eq? amount "cops called") (eq? check "cops called")) "cops called" "Incorrect password")
            (begin (withdraw pass1 amount) (send account deposit pass2 amount) amount))
      )
    )
  ))

(define make-accountlpats%
  (class make-accountlpat%
    (super-new)
    (init-field (rate 0.01))
    (inherit-field count balance password ledger)
    (inherit deposit withdraw view show transfer)
    (define/public (interest pass)     
      (let ((result (eq? pass password)))
        (if (equal? result #t) 
          (begin 
            (set! count 0) 
            (set! balance (+ balance (* balance rate))) 
            balance)
          (begin 
            (set! count (+ count 1)) 
            (if (>= count 8) "cops called" "Incorrect password"))
        )
      )
    )
  ))


;Test 
(printf "starting tests automatically\n\n")

(printf "*** test for section 1***\n")

(printf "(define acc (new make-account% [balance 100]))\n")
(define acc (new make-account% [balance 100]))
(printf "(send acc view) : ")
(send acc view)
(printf "(send acc withdraw 50) : ")
(send acc withdraw 50)
(printf "(send acc withdraw 60) : ")
(send acc withdraw 60)
(printf "(send acc deposit 40) : ")
(send acc deposit 40)
(printf "(send acc withdraw 60) : ")
(send acc withdraw 60)
(printf "(define acc2 (new make-account%))\n")
(define acc2 (new make-account%))
(printf "(send acc2 deposit 17) : ")
(send acc2 deposit 17)

(printf "\n*** test for section 2***\n")

(printf "(define accl (new make-accountl% [balance 100]))\n")
(define accl (new make-accountl% [balance 100]))
(printf "(send accl view) : ")
(send accl view)
(printf "(send accl withdraw 40) : ")
(send accl withdraw 40)
(printf "(send accl deposit 55) : ")
(send accl deposit 55)
(printf "(send accl show) : ")
(send accl show)

(printf "\n*** test for section 3***\n")

(printf "(define acclp (new make-accountlp% [balance 100][password 'secret]))\n")
(define acclp (new make-accountlp% [balance 100][password 'secret]))
(printf "(send acclp view 'secret) : ")
(send acclp view 'secret)
(printf "(send acclp withdraw 'secret 40) : ")
(send acclp withdraw 'secret 40)
(printf "(send acclp deposit 'rosebud 60) : ")
(send acclp deposit 'rosebud 60)
(printf "(send acclp show 'secret) : ")
(send acclp show 'secret)

(printf "\n*** test for section 4***\n")

(printf "(define acclpa (new make-accountlpa% [balance 100][password 'secret]))\n")
(define acclpa (new make-accountlpa% [balance 100][password 'secret]))
(printf "(send acclpa withdraw 'secret 60) : ")
(send acclpa withdraw 'secret 60)
(printf "(send acclpa withdraw 'rosebud 60) : ")
(send acclpa withdraw 'rosebud 60)
(printf "(send acclpa withdraw 'rosebud 60) : ")
(send acclpa withdraw 'rosebud 60)
(printf "(send acclpa withdraw 'rosebud 60) : ")
(send acclpa withdraw 'rosebud 60)
(printf "(send acclpa withdraw 'rosebud 60) : ")
(send acclpa withdraw 'rosebud 60)
(printf "(send acclpa withdraw 'rosebud 60) : ")
(send acclpa withdraw 'rosebud 60)
(printf "(send acclpa withdraw 'rosebud 60) : ")
(send acclpa withdraw 'rosebud 60)
(printf "(send acclpa withdraw 'rosebud 60) : ")
(send acclpa withdraw 'rosebud 60)
(printf "(send acclpa withdraw 'rosebud 60) : ")
(send acclpa withdraw 'rosebud 60)

(printf "\n*** test for section 5***\n")

(printf "(define from-acc (new make-accountlpat% [balance 300][password 'secret]))\n")
(define from-acc (new make-accountlpat% [balance 300][password 'secret]))
(printf "(define to-acc (new make-accountlpat% [balance 200][password 'rosebud]))\n")
(define to-acc (new make-accountlpat% [balance 200][password 'rosebud]))
(printf "(send from-acc transfer 'secret to-acc 'secret) : ")
(send from-acc transfer 'secret to-acc 'secret)
(printf "(send from-acc show 'secret) : ")
(send from-acc show 'secret)
(printf "(send from-acc transfer 'secret to-acc 'rosebud) : ")
(send from-acc transfer 'secret to-acc 'rosebud)
(printf "(send from-acc show 'secret) : ")
(send from-acc show 'secret)
(printf "(send to-acc show 'rosebud) : ")
(send to-acc show 'rosebud)

(printf "\n*** test for section 6***\n")

(printf "(define sav (new make-accountlpats% [balance 100][password 'rosebud][rate 5/100]))\n")
(define sav (new make-accountlpats% [balance 100][password 'rosebud][rate 5/100]))
(printf "(send sav interest 'bananas) : ")
(send sav interest 'bananas)
(printf "(send sav interest 'rosebud) : ")
(send sav interest 'rosebud)
(printf "(send sav deposit 'rosebud 0) : ")
(send sav deposit 'rosebud 0)

