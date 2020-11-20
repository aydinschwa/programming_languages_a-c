#lang racket
(define (stream-for-n-steps stream n)
  (letrec ([aux (lambda(stream n ans)
                  (let ([eval (stream)])
                    (if (= n 0)
                        ans
                        (aux (cdr eval) (- n 1) (cons (car eval) ans)))))])
    (reverse (aux stream n null))))

; Problem 1
(define (palindromic nums)
  (letrec ([f (lambda(nums n) 
    (let* ([l (list-ref nums n)]
           [r (list-ref nums (- (length nums) (+ 1 n)))]
           [sum (+ l r)])
      (if (equal? (+ 1 n) (length nums))
          (cons sum null)
          (cons sum (f nums (+ 1 n))))))])
    (f nums 0)))

; Problem 2
(define fibonacci
  (letrec ([f (lambda(x y) (cons x (lambda() (f y (+ x y)))))])
    (lambda() (f 0 1))))

; Problem 3
(define (stream-until f s)
  (if (f (car (s)))
      (stream-until f (cdr (s)))
      #f))

; Problem 4
(define (stream-map f s)
  (letrec ([aux (lambda(f s) (cons (f (car (s))) (lambda() (aux f (cdr (s))))))])
    (lambda() (aux f s))))

; Problem 5
(define (stream-zip s1 s2)
  (letrec ([aux (lambda(s1 s2) (cons (cons (car (s1)) (car (s2))) (lambda() (aux (cdr (s1)) (cdr (s2))))))])
    (aux s1 s2)))

; Problem 6
; You can't write a stream-reverse function because a stream has no end.
; You can't move the last element to the first because there is no last.

; Problem 7
(define (interleave streamlist)
  (letrec ([aux (lambda(streamlist n)
                  (let* ([select (modulo n (length streamlist))]
                         [sel-stream (list-ref streamlist select)])
                    (begin
                      (list-set streamlist select (cdr (sel-stream)))
                      (cons (car (sel-stream)) (lambda() (aux streamlist (+ 1 n)))))))])
    (lambda() (aux streamlist 0))))

(define nats
  (letrec ([f (lambda(x) (cons x (lambda() (f (+ 1 x)))))])
    (lambda() (f 1))))
          
                    
(define x (interleave (list nats)))
(stream-for-n-steps x 4)