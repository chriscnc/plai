#lang plai-typed

(define-type-alias Location number)

(define-type Binding
  [bind (name : symbol) (val : Location)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Storage
  [cell (location : Location) (val : Value)])

(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)


(define (lookup [n : symbol] [env : Env]) : Location
  (cond
    [(empty? env) (error n "name not found")]
    [else (cond 
	    [(symbol=? n (bind-name (first env))) 
	     (bind-val (first env))]
	    [else (lookup n (rest env))])]))


(define (fetch [loc : Location] [sto : Store]) : Value
  (cond
    [(empty? sto) (error 'loc "location not found")]
    [else (cond
	    [(equal? loc (cell-location (first sto)))
	     (cell-val (first sto))]
	    [else (fetch loc (rest sto))])]))


(define-type ExprC
  [numC  (n : number)]
  [varC   (s : symbol)]
  [appC  (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC  (arg : symbol) (body : ExprC)]
  [setC (var : symbol) (arg : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)]
  )

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

(define-type Result
  [v*s (v : Value) (s : Store)])

(define (interp [expr : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC expr
    [numC  (n) (v*s (numV n) sto)]
    [varC   (n) (v*s (fetch (lookup n env) sto) sto)]
    [plusC (l r) (type-case Result (interp l env sto)
		   [v*s (v-l s-l) 
			(type-case Result (interp r env s-l)
			  [v*s (v-r s-r) 
			       (v*s (num+ v-l v-r) s-r)])])]
    [multC (l r) (type-case Result (interp l env sto)
	           [v*s (v-l s-l)
			(type-case Result (interp r env s-l)
			  [v*s (v-r s-r)
			       (v*s (num* v-l v-r) s-r)])])]
    [lamC  (a b) (v*s (closV a b env) sto)]
    [setC (var val) (type-case Result (interp val env sto)
		      [v*s (v-val s-val)
			   (let ([where (lookup var env)]) ; get the l-value
			     (v*s v-val
				  (override-store (cell where v-val)
						  s-val)))])]
    [seqC (b1 b2) (type-case Result (interp b1 env sto)
	            [v*s (v-b1 s-b1)
			 (interp b2 env s-b1)])]
    [appC (f a) (type-case Result (interp f env sto)
		  [v*s (v-f s-f) 
		       (type-case Result (interp a env s-f)
		         [v*s (v-a s-a)
			      (let ([where (new-loc)])
				(interp (closV-body v-f)
					(extend-env (bind (closV-arg v-f) where)
						    (closV-env v-f))
					(override-store (cell where v-a) s-a)))])])]
    ))


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


; This is cheating, relying on box to implement locations
; for boxes. To eliminate, would have to thread the next
; available location through the interpreter.
(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
	(set-box! n (add1 (unbox n)))
	(unbox n)))))


;(define (parse [s : s-expression]) : ExprC
;  (cond
;    [(s-exp-number? s) (numC (s-exp->number s))]
;    [(s-exp-symbol? s) (varC (s-exp->symbol s))]
;    [(s-exp-list? s)
;     (let ([sl (s-exp->list s)])
;       (cond 
;	 [(s-exp-list? (first sl))
;	  (appC (parse (first sl)) (parse (second sl)))]
;	 [else (case (s-exp->symbol (first sl))
;		 [(+) (plusC (parse (second sl)) (parse (third sl)))]
;		 [(*) (multC (parse (second sl)) (parse (third sl)))]
;		 [(lambda) (lamC (s-exp->symbol (first (s-exp->list (second sl))))
;				 (parse (third sl)))]
;		 )]))]
;    [else (error 'parse "invalid input")]))
