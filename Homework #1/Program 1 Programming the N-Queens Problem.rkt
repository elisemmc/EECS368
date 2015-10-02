#lang racket

; 1.) Enumerate Interval

(define (enumerate-interval low high)
  (cond [(>= high low) (cons low (enumerate-interval (+ low 1) high))]
                [else '()]
  )
)

; 2.) iPerm
(define (iperm L A)
  (cond 
    [(null? L) (list A)] 
    [else (append-map (lambda (x) (iperm (remove x L) (cons x A))) L)]
  )
)

;

  
                  