
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
    (reverse (aux stream n null))))

; Problem 5
(define funny-number-stream
  (letrec ([f (lambda(x) (cons x (lambda()
                                   (if (= 4 (modulo x 5))
                                       (f (-(+ x 1)))
                                       (f (+ (abs x) 1))))))])
    (lambda() (f 1))))

; Problem 6
(define dan-then-dog
  (letrec ([f (lambda(x n) (cons x (lambda()
                                   (if (= 1 (modulo n 2))
                                       (f "dog.jpg" (+ 1 n))
                                       (f "dan.jpg" (+ 1 n))))))])
    (lambda() (f "dan.jpg" 1))))

; Problem 7
(define (stream-add-zero stream)
  (lambda()
    (let ([st (stream)])
      (cons (cons 0 (car st)) (stream-add-zero(cdr st))))))

; Problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda(xs ys n)
                (let ([x (list-nth-mod xs n)]
                      [y (list-nth-mod ys n)])
                  (cons (cons x y) (lambda() (f xs ys (+ 1 n))))))])
    (lambda() (f xs ys 0))))

; Problem 9
(define (vector-assoc v vec)
  (letrec ([f (lambda(v vec n)
    (let ([curr (vector-ref vec n)])
       (cond [(equal? (vector-length vec) (+ 1 n)) #f]
             [(pair? curr) (if (equal? v (car curr))
                            curr
                            (f v vec (+ 1 n)))]
             [#t (f v vec (+ 1 n))])))])
    (f v vec 0)))

; Problem 10
(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [next 0])
  (if (vector-assoc n cache)
      (lambda(x) (vector-assoc x cache))
      (begin
        (vector-set! cache next (assoc n xs))
        (set! next (modulo (+ 1 next) n))
        (lambda(x) (vector-assoc x cache))))))
      
  

 
                        
                                  

              



