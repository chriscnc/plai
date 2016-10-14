#lang plai-typed

(define-type ExprC
  [numC  (n : number)]
  [idC   (s : symbol)]
  [appC  (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])


(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])


(define-type Binding
  [bind (name : symbol) (val : number)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)


(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [(*) (multC (parse (second sl)) (parse (third sl)))]
         [else (appC (s-exp->symbol (first sl)) (parse (second sl)))]))]
    [else (error 'parse "invalid input")]))


(define (lookup [n : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error n "name not found")]
    [else (cond 
	    [(symbol=? n (bind-name (first env))) 
		         (bind-val (first env))]
	    [else (lookup n (rest env))])]))


(define (interp [e : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC  (n) n]
    [idC   (n) (lookup n env)]
    [appC  (f a) (local ([define fd (get-fundef f fds)])
		   (interp (fdC-body fd)
			   (extend-env (bind (fdC-arg fd) (interp a env fds))
				       mt-env)
			   fds))]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]))


(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds)  (cond
		    [(equal? n (fdC-name (first fds))) (first fds)]
		    [else (get-fundef n (rest fds))])]))


(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
	     [numC (n) in]
	     [idC  (s) (cond
			 [(symbol=? s for) what]
			 [else in])]
	     [appC (f a) (appC f (subst what for a))]
	     [plusC (l r) (plusC (subst what for l) 
				 (subst what for r))]
	     [multC (l r) (multC (subst what for l) 
				 (subst what for r))]))


