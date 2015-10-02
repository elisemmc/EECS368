#lang racket
;Elise McEllhiney
;EECS 368
;Homework # 1
;N-Queens Problem

(define (enumerate-interval low high)
  (cond [(>= high low) (cons low (enumerate-interval (+ low 1) high))]
        [else '()]
  )
)

(define (iperm L A)
  (cond 
    [(null? L) (list A)] 
    [else (append-map (lambda (x) (iperm (remove x L) (cons x A))) L)]
  )
)

(define (diagcheck col A D)
  (cond
    [(null? A) #f]
    [(= col (- (first A) D)) #t]
    [(= col (+ (first A) D)) #t]
    [else (diagcheck col (cdr A) (+ D 1))]
  )
)

(define (diag? col A)
  (diagcheck col A 1)
)

(define (iqueens L A)
  (cond 
    [(null? L) (list A)] 
    [else (append-map (lambda (x) (cond [(diag? x A) '()]
                                        [else (iqueens (remove x L) (cons x A))]
                                  ))L)]))

(define (q n)
  (iqueens (enumerate-interval 1 n) '())
)

(define (test n)
  (map [lambda(x) (list x (length (q x)))] (enumerate-interval 0 n))
)


(test 13)
                  