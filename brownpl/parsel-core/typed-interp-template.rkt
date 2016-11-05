#lang plai-typed

(require "typed-lang2.rkt")

(define-type Cell
  [cell (location : Location) (value : ValueC)])

(define-type-alias Store (listof Cell))
(define override-store cons)

(define-type Result
  [v*s (value : ValueC) (store : Store)])

;(define-type ValueC
;  [ObjectV (fields : (listof FieldV))]
;  [ClosureV (args : (listof symbol)) (body : ExprC) (env : Env)]
;  [NumV (n : number)]
;  [StrV (s : string)]
;  [TrueV]
;  [FalseV])

(define (interp-fargs [fargs : (listof ExprC)] [env : Env] [store : Store]) : (listof Result)
  (cond
    [(empty? fargs) '()]
    [else (type-case Result (interp-full (first fargs) env store)
            [v*s (v s) (cons (v*s v s) 
			     (interp-fargs (rest fargs) env s))])]))


(define (interp-full [exprC : ExprC] [env : Env] [store : Store]) : Result
  (type-case ExprC exprC
    [NumC (n) (v*s (NumV n) store)]
    [StrC (s) (v*s (StrV s) store)]
    [TrueC () (v*s (TrueV) store)]
    [FalseC () (v*s (FalseV) store)]

;    [ObjectC (fields : (listof FieldC))]
;    [GetFieldC (obj : ExprC) (field : ExprC)]
;    [SetFieldC (obj : ExprC) (field : ExprC) (value : ExprC)]

    [FuncC (args body) (v*s (ClosureV args body env) store)]

;    [AppC (func : ExprC) (args : (listof ExprC))]
    ; start by assuming we don't need to thread the store through
    ; all the arg
    [AppC (func args) (let ([results (interp-fargs args env store)])
			(interp-full body 
				     (foldl extend-env )
				     (v*s-store (first results))))]
    [LetC (id expr body) (type-case Result (interp-full expr env store)
			   [v*s (v-bind s-bind)
				(let ([loc (new-loc)])
				  (interp-full body
					       (extend-env (bind id loc) env)
					       (override-store (cell loc v-bind) s-bind)))])]
    [IdC (id) (v*s (fetch (lookup id env) store) store)]
    [Set!C (id val) (type-case Result (interp-full val env store)
		        [v*s (v-val s-val)
			     (v*s v-val
				  (override-store (cell (lookup id env) v-val)
						  s-val))])]
    [IfC (c t e) (type-case Result (interp-full c env store)
			    [v*s (v-c s-c) 
				 (type-case ValueC v-c
				   [FalseV () (interp-full e env s-c)]
				   [else (interp-full t env s-c)])])]
    [SeqC (e1 e2) (type-case Result (interp-full e1 env store)
		    [v*s (v-e1 s-e1) 
			 (interp-full e2 env s-e1)])]
    [ErrorC (expr) (type-case Result (interp-full expr env store)
	             [v*s (v s)
			  (type-case ValueC v
		            [StrV (s) (interp-error s)]
			    [else (interp-error "Malformed ErrorC")])])]
    ; The core operations are 'string+ 'num+ 'num- '== '< '> 'print 'tagof
    [Prim1C (op arg)
	    (case op
	      [(print) (type-case Result (interp-full arg env store)
			 [v*s (v s)
			      (begin
				(display (pretty-value v))
				(v*s v s))])]
	      [(tagof) (type-case Result (interp-full arg env store)
		         [v*s (v s)
			      (v*s (StrV (tagof v)) store)])]
	      )]
    [Prim2C (op arg1 arg2)
	    (case op
	      [(num+) (type-case Result (interp-full arg1 env store)
		        [v*s (v-arg1 s-arg1)
			     (type-case Result (interp-full arg1 env s-arg1)
			       [v*s (v-arg2 s-arg2)
				    (v*s (num+ v-arg1 v-arg2) s-arg2)])])]
	      [(num-) (type-case Result (interp-full arg1 env store)
		        [v*s (v-arg1 s-arg1)
			     (type-case Result (interp-full arg1 env s-arg1)
			       [v*s (v-arg2 s-arg2)
				    (v*s (num- v-arg1 v-arg2) s-arg2)])])]
	      [(==) (type-case Result (interp-full arg1 env store)
		      [v*s (v-arg1 s-arg1)
			   (type-case Result (interp-full arg1 env s-arg1)
		             [v*s (v-arg2 s-arg2)
				  ; Racket's equal semantics appear to be equivalent 
				  ; to the spec.
				  (if (equal? v-arg1 v-arg2)
				    (v*s (TrueV) s-arg2)
				    (v*s (FalseV) s-arg2))])])]
	      [(<) (type-case Result (interp-full arg1 env store)
		      [v*s (v-arg1 s-arg1)
			   (type-case Result (interp-full arg1 env s-arg1)
		             [v*s (v-arg2 s-arg2)
				  ; Racket's equal semantics appear to be equivalent 
				  ; to the spec.
				  (if (numlt? v-arg1 v-arg2)
				    (v*s (TrueV) s-arg2)
				    (v*s (FalseV) s-arg2))])])]
	      [(>) (type-case Result (interp-full arg1 env store)
		      [v*s (v-arg1 s-arg1)
			   (type-case Result (interp-full arg1 env s-arg1)
		             [v*s (v-arg2 s-arg2)
				  ; Racket's equal semantics appear to be equivalent 
				  ; to the spec.
				  (if (numgt? v-arg1 v-arg2)
				    (v*s (TrueV) s-arg2)
				    (v*s (FalseV) s-arg2))])])]
	      [else (interp-error "Haven't covered op yet")])]
    [else (interp-error (string-append "Haven't covered a case yet:"
                                       (to-string exprC)))]))

(define (interp [exprC : ExprC]) : ValueC
  (type-case Result (interp-full exprC empty empty)
    [v*s (value store) value]))


(define (tagof [v : ValueC]) : string
  (type-case ValueC v
    [ObjectV (fields) "object"]
    [ClosureV (args body env) "function"]
    [NumV (n) "number"]
    [StrV (s) "string"]
    [TrueV () "boolean"]
    [FalseV () "boolean"]))
    

(define (numgt? [arg1 : ValueC] [arg2 : ValueC]) : boolean
  (if (and (NumV? arg1) 
	   (NumV? arg2))
    (> (NumV-n arg1) (NumV-n arg2))
    (interp-error "Bad arguments to >")))

(define (numlt? [arg1 : ValueC] [arg2 : ValueC]) : boolean
  (if (and (NumV? arg1) 
	   (NumV? arg2))
    (< (NumV-n arg1) (NumV-n arg2))
    (interp-error "Bad arguments to <")))

(define (num+ [arg1 : ValueC] [arg2 : ValueC]) : ValueC
  (if (and (NumV? arg1) 
	   (NumV? arg2))
    (NumV (+ (NumV-n arg1) 
	     (NumV-n arg2)))
    (interp-error "Bad arguments to +")))

(define (num- [arg1 : ValueC] [arg2 : ValueC]) : ValueC
  (if (and (NumV? arg1) 
	   (NumV? arg2))
    (NumV (- (NumV-n arg1) 
	     (NumV-n arg2)))
    (interp-error "Bad arguments to -")))


(define (lookup [name : symbol] [env : Env]) : Location
  (cond 
    [(empty? env) (error name "name not found")]
    [else (cond
	    [(symbol=? name (bind-name (first env)))
	     (bind-value (first env))]
	    [else (lookup name (rest env))])]))

(define (fetch [loc : Location] [store : Store]) : ValueC
  (cond
    [(empty? store) (error 'loc "location not found")]
    [else (cond
	    [(equal? loc (cell-location (first store)))
	     (cell-value (first store))]
	    [else (fetch loc (rest store))])]))

(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
	(set-box! n (add1 (unbox n)))
	(unbox n)))))
	
(define extend-env cons)
(define mt-env empty)

;; a start to implementing equal semantics explicitly rather than
;; relying on Racket's equal semantics
;(define (equal [v1 : ValueC] [v2 : ValueC]) : boolean
;  (type-case ValueC v1
;    [StrV (s1) (type-case ValueC v2
;	         [StrV (s2) (string=? s1 s2)]
;		 [else false])]
;    [NumV (n1) (type-case ValueC v2
;	         [NumV (n2) (= n1 n2)]
;		 [else false])]
;    [TrueV () (type-case ValueC v2
;		[TrueV () true]
;		[else false])]
;    [FalseV () (type-case ValueC v2
;		 [FalseV () true]
;		 [else false])]
;    [ObjectV (fs1) (type-case ValueC v2
;		     [ObjectV (fs2) (fields-equal fs1 fs2)])]
;    [ClosureV (args body env) 
;    ))
;
;(define (fields-equal [fs1 : (listof FieldV)] [fs2 : (listof FieldV)]) : boolean
;  (cond
;    [(and (empty? fs1) (empty? fs2)) true]
;    [(and (empty? fs1) (not (empty? fs2))) false]
;    [(and (not empty? fs1) (empty? fs)) false]
;    [else (let ([f1 (first fs1)]
;		[f2 (first fs2)])
;	    (if (and (string=? (fieldV-name f1) 
;			       (fieldV-name f2))
;		     (equal (fieldV-value f1)
;			    (fieldV-value f2)))
;	      (fields-equal (rest fs1) (rest fs2))
;	      false))]))

