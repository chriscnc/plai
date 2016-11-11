#lang plai-typed


(define-type Binding
  [bind (name : symbol) (typ : Type)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (lookup [n : symbol] [env : Env]) : Type
  (cond
    [(empty? env) (error n "name not found")]
    [else (cond 
	    [(symbol=? n (bind-name (first env))) 
	     (bind-typ (first env))]
	    [else (lookup n (rest env))])]))


(define-type ExprC
  [numC  (n : number)]
  [idC   (s : symbol)]
  [appC  (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC  (arg : symbol) (argT : Type) (retT : Type) (body : ExprC)] )

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])


(define-type Type
  [numT]
  [funT (arg : Type) (ret : Type)])


(define (tc [expr : ExprC] [env : Env]) : Type
  (type-case ExprC expr
    [numC  (n) (numT)]
    [idC   (n) (lookup n env)]
    [plusC (l r) (let ([lt (tc l env)]
		       [rt (tc r env)])
		   (if (and (equal? lt (numT))
			    (equal? rt (numT)))
		     (numT)
		     (error 'tc "+ type error")))]
    [multC (l r) (let ([lt (tc l env)]
		       [rt (tc r env)])
		   (if (and (equal? lt (numT))
			    (equal? rt (numT)))
		     (numT)
		     (error 'tc "* type error")))]
    [lamC (a argT retT b) 
	  (if (equal? (tc b (extend-env (bind a argT) env)) retT)
	    (funT argT retT)
	    (error 'tc "lam type mismatch"))]
    [appC (f a) (let ([ft (tc f env)]
		      [at (tc a env)])
		  (cond 
		    [(not (funT? ft))
		     (error 'tc "non function type in function position")]
		    [(not (equal? (funT-arg ft) at))
		     (error 'tc "app arg type mismatch")]
		    [else (funT-ret ft)]))]
    ))


