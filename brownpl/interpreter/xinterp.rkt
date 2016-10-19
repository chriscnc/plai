#lang plai-typed

(print-only-errors true)

(define-type Binding
  [binding (name : symbol) (named-expr : CFWAE)])

(define-type CFWAE
  [num   (n : number)]
  [binop (op : symbol) (lhs : CFWAE) (rhs : CFWAE)]
  [with  (lob : (listof Binding)) (body : CFWAE)]
  [id    (name : symbol)]
  [if0   (c : CFWAE) (t : CFWAE) (e : CFWAE)]
  [fun   (args : (listof symbol)) (body : CFWAE)]
  [app   (f : CFWAE) (args : (listof CFWAE))])

(define-type Env
  [mtEnv]
  [anEnv (name : symbol) (value : CFWAE-Value) (env : Env)])


(define-type CFWAE-Value
  [numV     (n : number)]
  [closureV (params : (listof symbol))
            (body : CFWAE)
            (env : Env)])


(define (extend-env [b : Binding] [env : Env]) : Env
  (let ([name (binding-name b)]
	[expr-val (interp (binding-named-expr b) env)])
  (anEnv name expr-val env)))

;; parse : expression -> CFWAE
; This procedure parses an expression into a CFWAE
(define (parse [sexp : s-expression]) : CFWAE
  (cond
    [(s-exp-number? sexp) (num (s-exp->number sexp))]
    [(s-exp-symbol? sexp) (id (s-exp->symbol sexp))]
    [(s-exp-list? sexp) 
     (let ([sl (s-exp->list sexp)])
       (case (s-exp->symbol (first sl))
	 [(+) (binop '+ (parse (second sl)) (parse (third sl)))]
	 [(-) (binop '- (parse (second sl)) (parse (third sl)))]
	 [(*) (binop '* (parse (second sl)) (parse (third sl)))]
	 [(/) (binop '/ (parse (second sl)) (parse (third sl)))]
	 [(if0) (if0 (parse (second sl)) 
		     (parse (third sl)) 
		     (parse (fourth sl)))]
	 [(fun) (let ([args (s-exp->list (second sl))]
		      [body (parse (third sl))])
		  (fun (map s-exp->symbol args) body))]
	 [(with) (let* ([id-expr-pairs (s-exp->list (second sl))]
			[bindings (map pair->Binding id-expr-pairs)]
			[body (parse (third sl))])
		   (with bindings body))]
	 [else (app (parse (first sl))
		    (map parse (rest sl)))]
	 ))]
    [else (error 'parse "Parse error")]))


(define (pair->Binding [pair : s-expression]) : Binding
  (let ([id (first (s-exp->list pair))]
	[expr (second (s-exp->list pair))])
    (binding (s-exp->symbol id) (parse expr))))


(define (lookup-op [op-sym : symbol]) : (number number -> number)
  (case op-sym
    ['+ +]
    ['- -]
    ['* *]
    ['/ /]
    [else (error 'lookup-op "Invalid operation")]))


(define (lookup-env [name : symbol] [env : Env]) : CFWAE-Value
  (type-case Env env
    [mtEnv () (error 'lookup-env "Symbol not found")]
    [anEnv (n v e) (if (symbol=? name n)
		     v
		     (lookup-env name e))]))


;;(define-type CFWAE
;;  [app   (f : CFWAE) (args : (listof CFWAE))])

;; interp : CFWAE -> CFWAE-Value
;; This procedure evaluates a CFWAE expression, producing a CFWAE-Value.
(define (interp [expr : CFWAE] [env : Env]) : CFWAE-Value
  (type-case CFWAE expr
    [num (n) (numV n)]
    [id (name) (lookup-env name env)]
    [binop (op-sym lhs rhs) (let ([op-f (lookup-op op-sym)]
				  [lhs-val (interp lhs env)]
				  [rhs-val (interp rhs env)])
			      (if (and (numV? lhs-val)
				       (numV? rhs-val))
				(if (zero? (numV-n rhs-val))
				  (error 'binop "Division by zero")
				  (numV (op-f (numV-n lhs-val) 
					      (numV-n rhs-val))))
				(error 'binop "Arguments not numbers")))]
    [if0 (c t e) (let ([c-val (interp c env)])
		   (if (numV? c-val)
		     (if (zero? (numV-n c-val))
		       (interp t env)
		       (interp e env))
		     (error 'if0 "test expression not a number")))]
    [with (lob body) (let* ([names (map binding-name lob)]
			    [unique-names (distinct names)])
		       (if (equal? (length names)
				   (length unique-names))
			 (let ([new-env (foldr extend-env env lob)])
			   (interp body new-env))
			 (error 'with "duplicate binding")))]
    [fun (args body) (closureV args body env)]
    [app (f args) (let* ([c (interp f env)]
			 [params (closureV-params c)]
			 [body (closureV-body c)]
			 [c-env (closureV-env c)]
;			 [evald-args (map (lambda (arg) (interp arg env)) args)]
			 [bindings (map2 binding params args)]
			 [new-env (foldr extend-env env bindings)])
		    (interp body new-env))]
    ))



(define (distinct [lst : (listof 'a)]) : (listof 'a)
  (letrec ([f (lambda (lst acc)
	     (cond 
	       [(empty? lst) acc]
	       [else (if (member (first lst) acc)
		       (f (rest lst) acc)
		       (f (rest lst) (cons (first lst) acc)))]))])
    (reverse (f lst (list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

;; testing helper
(define (eval x)
  (interp (parse x) (mtEnv)))

;; test interpreter
(test (numV 42) (eval '42)) 
(test (numV 9) (eval '{/ {+ {* 4 4} {- 4 2}} 2}))
(test/exn (eval '{/ 7 0}) "Division by zero")
;; need tests for when you pass non-number args to a binop
(test (numV 1) (eval '{if0 0 1 2}))
(test (numV 2) (eval '{if0 1 1 2}))
;(test/exn (parse '{if0 {fun {x} {x}} 1 2}) "test expression not a number")
(test (numV 7) (eval '{with {{x 2} {y 3}} 
	           {with {{z {+ x y}}}
		     {+ x z}}}))
(test/exn (eval '{with {{x 2} {x 3}} 
		     {+ x 2}}) "duplicate binding")
(test (numV 6) (eval '{{fun {x y} {* x y}} 2 3}))

;; test distinct
(test (list 'a 'b) (distinct (list 'a 'a 'b 'b)))

;; test lookup-op
(test 3 ((lookup-op '+) 1 2))
(test 3 ((lookup-op '-) 4 1))
(test 3 ((lookup-op '*) 1 3))
(test 3 ((lookup-op '/) 9 3))

;; test lookup-env
(test/exn (lookup-env 'x (mtEnv)) "Symbol not found")
(test (numV 42) (lookup-env 'x (anEnv 'x (numV 42) (mtEnv))))
(test (numV 42) (lookup-env 'x (anEnv 'x (numV 42) 
				      (anEnv 'x (numV 32) (mtEnv)))))
(test (numV 24) (lookup-env 'y (anEnv 'x (numV 42) 
				      (anEnv 'y (numV 24) (mtEnv)))))

;; test parse
(test (num 42) (parse '42))
(test (binop '+ (num 42) (num 42)) (parse '{+ 42 42}))
(test (binop '- (num 42) (num 42)) (parse '{- 42 42}))
(test (binop '* (num 42) (num 42)) (parse '{* 42 42}))
(test (binop '/ (num 42) (num 42)) (parse '{/ 42 42}))
(test (if0 (num 0) (num 1) (num 2)) (parse '{if0 0 1 2}))
(test (if0 (num 0) (num 1) (num 2)) (parse '{if0 0 1 2}))
(test (if0 (fun (list 'x 'y) (binop '* (id 'x) (id 'y))) (num 1) (num 2))
      (parse '{if0 {fun {x y} {* x y}} 1 2})) 
(test (fun (list 'x 'y) (binop '+ (id 'x) (id 'y))) (parse '{fun {x y} {+ x y}}))
(test (with (list (binding 'x (num 1)) 
		  (binding 'y (num 2))) (binop '+ (id 'x) (id 'y)))
      (parse '{with {{x 1} {y 2}} {+ x y}}))
(test (app (id 'f) (list (num 1) (num 2)))
      (parse '{f 1 2}))
