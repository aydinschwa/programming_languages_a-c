;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
(define (racketlist->mupllist li)
  (if (null? li)
      (aunit)
      (apair (car li) (racketlist->mupllist (cdr li)))))

(define (mupllist->racketlist li)
  (if (aunit? li)
      null
      (cons (apair-e1 li) (mupllist->racketlist (apair-e2 li)))))

;; Problem 2
;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you. 
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(ifgreater? e)
         (let ([comp1 (eval-under-env (ifgreater-e1 e) env)]
               [comp2 (eval-under-env (ifgreater-e2 e) env)]
               [ans1 (eval-under-env (ifgreater-e3 e) env)]
               [ans2 (eval-under-env (ifgreater-e4 e) env)])
           (if (and (int? comp1) (int? comp2))
                (if (> (int-num comp1) (int-num comp2))
                    ans1
                    ans2)
                (error "MUPL ifgreater applied to non-number")))]
        [(fun? e)
         (closure env e)]
        [(call? e)
         (let* ([close (call-funexp e)]
                [fun (closure-fun close)]
                [val (call-actual e)])
           (if (closure? close)
               (let* ([arg (fun-formal fun)]
                     [body (fun-body fun)]
                     [add-env1 (cons arg val)]
                     [add-env2 (cons (fun-nameopt fun) env)])
                 (eval-under-env body (append (cons add-env1 add-env2) env)))
               (error "call was not passed a function closure")))]
        [(mlet? e)
         (let* ([var-val (eval-under-env (mlet-e e) env)]
                [lcl-env (cons (cons (mlet-var e) var-val) env)])
           (eval-under-env (mlet-body e) lcl-env))]
        [(apair? e)
         (let ([car (eval-under-env (apair-e1 e) env)]
               [cdr (eval-under-env (apair-e2 e) env)])
           (apair car cdr))]
        [(fst? e)
         (if (apair? (fst-e e))
             (eval-under-env (apair-e1 (fst-e e)) env)
             (error "Called fst on non-pair"))]
        [(snd? e)
         (if (apair? (snd-e e))
             (eval-under-env (apair-e2 (snd-e e)) env)
             (error "Called snd on non-pair"))]
        [(aunit? e) e]
        [(isaunit? e)
         (let ([val (eval-under-env (isaunit-e e) env)])
           (if (aunit? val)
               (int 1)
               (int 0)))]
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3
(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (empty? lstlst)
      e2
      (mlet (caar lstlst) (cdar lstlst) (mlet* (cdr lstlst) e2))))
          
(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4
                    (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

;;(define mupl-map (

;;(define mupl-mapAddN 
  ;;(mlet "map" mupl-map
        ;;"CHANGE (notice map is now in MUPL scope)"))


