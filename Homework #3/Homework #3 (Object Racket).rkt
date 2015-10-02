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
    (init-field (password 'secret))
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