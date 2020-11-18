
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
(define (stream-for-n-steps stream n)
  (letrec ([aux (lambda(stream n ans)
                  (let ([eval (stream)])
                    (if (= n 0)
                        ans
                        (aux (cdr eval) (- n 1) (cons (car eval) ans)))))])
    (aux stream n null)))

(define ones (lambda() (cons 1 ones)))

(define nats
  (letrec ([f (lambda(x) (cons x (lambda() (f (+ x 1)))))])
    (lambda() (f 1))))

; Problem 5
(define funny-number-stream
  (letrec ([f (lambda(x) (cons x (lambda()
                                   (if (= 4 (modulo x 5))
                                       (f (-(+ x 1)))
                                       (f (+ (abs x) 1))))))])
    (lambda() (f 1))))

; Problem 6
(define dan-then-dog


  




