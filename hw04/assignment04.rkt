
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

; Problem 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

; Problem 2
(define (string-append-map xs suffix)
  (map (lambda(x) (string-append x suffix)) xs))

; Problem 3
(define (list-nth-mod xs n)
  (cond [(empty? xs) raise "list-nth-mod: empty list"]
        [(negative? n) raise "list-nth-mod: negative number"]
        [#t (list-ref xs (remainder n (length xs)))]))

; Problem 4 
  
  




