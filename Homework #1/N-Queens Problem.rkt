#lang racket

;Elise McEllhiney
;EECS 368
;Homework 1
;N-Queens Problem

; 1.) Enumerate Interval
;enumerates interval from low input to high input
(define (enumerate-interval low high)
  (cond [(>= high low) (cons low (enumerate-interval (+ low 1) high))]
        [else '()]
  )
)

; 2.) iPerm
;gives all permutations of an inputted list
(define (iperm L A)
  (cond 
    [(null? L) (list A)] 
    [else (append-map (lambda (x) (iperm (remove x L) (cons x A))) L)]
  )
)

; 3.) Diagonal Checker
;checks that the new number is not in a diagonal line to any of the previous placements
;diagcheck runs and has and extra value D that keeps track of how far the value we're checking is from the beginning of the list
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
  
; 4.) Queens
#|
trial and error

(define (iqueens-inner L A)
  (cond 
    [(null? L) (list A)] 
    [else (append-map (lambda (x) (iqueens-inner (remove x L) (cond
                                                                 [(diag? x A) null]
                                                                 [else (cons x A)]
                                                              ))) L)
    ]
  )
)

;removes all the extra crap that i get out of putting my conditional at the end, my conditional returns lots of short lists as well
(define (iqueensx L A)
  (filter (lambda(x)(= (length L) (length x))) (iqueens-inner L A))
)
|#

;combines the permutation function and the diagonal function to get the possible arrangements of queens on a chess board of size N x N
(define (iqueens L A)
  (cond 
    [(null? L) (list A)] 
    [else (append-map (lambda (x) (cond [(diag? x A) '()]
                                        [else (iqueens (remove x L) (cons x A))]
                                  ))L)]))


; 5:) Q
;takes input n and runs iqueens without having to type out the list and input a null list
(define (q n)
  (iqueens (enumerate-interval 1 n) '())
)

; 6:) test program
;counts the number of arrangements that queens can be arranges on an N x N board and outputs (N number-of-permutations)
(define (test n)
  (map [lambda(x) (list x (length (q x)))] (enumerate-interval 0 n))
)
                  