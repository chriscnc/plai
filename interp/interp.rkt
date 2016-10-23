#lang plai-typed

(define-type ExprC
  [numC  (n : number)]
  [idC   (s : symbol)]
  [appC  (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC  (arg : symbol) (body : ExprC)])

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))

(define (mt-env) : Env 
  (list ))

(define (extend-env [x : Binding] [e : Env]) : Env 
  (cons x e))


(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [(*) (multC (parse (second sl)) (parse (third sl)))]
	 [(lambda) (lamC (s-exp->symbol (first (s-exp->list (second sl))))
			 (parse (third sl)))]
         [else (appC (parse (first sl)) (parse (second sl)))]))]
    [else (error 'parse "invalid input")]))


(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
      (error 'num+ "one argument was not a number")]))


(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
      (error 'num* "one argument was not a number")]))


(define (lookup [n : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error n "name not found")]
    [else (cond 
	    [(symbol=? n (bind-name (first env))) 
		         (bind-val (first env))]
	    [else (lookup n (rest env))])]))


(define (interp [expr : ExprC] [env : Env]) : Value
  (type-case ExprC expr
    [numC  (n) (numV n)]
    [idC   (n) (lookup n env)]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [lamC  (args body) (closV args body env)]
    [appC  (f a) (local ([define f-value (interp f env)])
		   (interp (closV-body f-value)
			   (extend-env (bind (closV-arg f-value) 
					     (interp a env))
				       (closV-env f-value))))]
    ))

;(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
;  (cond
;    [(empty? fds) (error 'get-fundef "reference to undefined function")]
;    [(cons? fds)  (cond
;		    [(equal? n (fdC-name (first fds))) (first fds)]
;		    [else (get-fundef n (rest fds))])]))


;(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
;  (type-case ExprC in
;	     [numC (n) in]
;	     [idC  (s) (cond
;			 [(symbol=? s for) what]
;			 [else in])]
;	     [appC (f a) (appC f (subst what for a))]
;	     [plusC (l r) (plusC (subst what for l) 
;				 (subst what for r))]
;	     [multC (l r) (multC (subst what for l) 
;				 (subst what for r))]))


