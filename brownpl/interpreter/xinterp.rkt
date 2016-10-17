#lang plai-typed
;; login : <YOUR-PIN-HERE>

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
	 [(if) (if0 (parse (second sl)) 
		    (parse (fourth sl)) 
		    (parse (list-ref sl 5)))]
	 [(fun) (let ([args (s-exp->list (second sl))]
		      [body (parse (third sl))])
		  (fun (map s-exp->symbol args) body))]
	 [(with) (let* ([id-expr-pairs (s-exp->list (second sl))]
			[bindings (map pair->Binding id-expr-pairs)]
			[body (parse (third sl))])
		   (with bindings body))] 
	 ))]
    [else (error 'parse "Parse error")]))


(define (pair->Binding [pair : s-expression]) : Binding
  (let ([id (first (s-exp->list pair))]
	[expr (second (s-exp->list pair))])
    (binding (s-exp->symbol id) (parse expr))))

;; interp : CFWAE -> CFWAE-Value
;; This procedure evaluates a CFWAE expression, producing a CFWAE-Value.
;(define (interp expr)
;  (...))

(print-only-errors true)

(test (num 42) (parse '42))
(test (binop '+ (num 42) (num 42)) (parse '{+ 42 42}))
(test (binop '- (num 42) (num 42)) (parse '{- 42 42}))
(test (binop '* (num 42) (num 42)) (parse '{* 42 42}))
(test (binop '/ (num 42) (num 42)) (parse '{/ 42 42}))
(test (if0 (num 0) (num 1) (num 2)) (parse '{if 0 then 1 else 2}))
(test (if0 (num 0) (num 1) (num 2)) (parse '{if 0 then 1 else 2}))
; should cause error during evaluation
(test (if0 (fun (list 'x 'y) (binop '* (id 'x) (id 'y))) (num 1) (num 2))
      (parse '{if {fun {x y} {* x y}} then 1 else 2})) 
(test (fun (list 'x 'y) (binop '+ (id 'x) (id 'y))) (parse '{fun {x y} {+ x y}}))
(test (with (list (binding 'x (num 1)) 
		  (binding 'y (num 2))) (binop '+ (id 'x) (id 'y)))
      (parse '{with {{x 1} {y 2}} {+ x y}}))
	     

