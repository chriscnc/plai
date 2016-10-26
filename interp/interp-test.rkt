#lang plai-typed

(require "interp.rkt")
(print-only-errors true)

;; test interp
(define (interp-ast-res [x : ExprC]) : Result
  (interp x mt-env mt-store))

(define (interp-ast-val [x : ExprC]) : Value
  (v*s-v (interp x mt-env mt-store)))

;(define-type Result
;  [v*s (v : Value) (s : Store)])

(test (numV 42) 
      (interp-ast-val (numC 42)))

(test (numV 3)
      (interp-ast-val (plusC (numC 1)
			     (numC 2))))

(test (numV 2)
      (interp-ast-val (multC (numC 1)
			     (numC 2))))


; (+ 10 ((lambda (_) 5) 10))
(test (numV 15)
      (interp-ast-val (plusC (numC 10)
			     (appC (lamC '_ (numC 5))
				   (numC 10)))))

; (lambda (x) ((lambda (y) (+ x y)) 4) 3)
(test (numV 7) 
      (interp-ast-val (appC (lamC 'x 
				  (appC (lamC 'y 
					      (plusC (idC 'x)
						     (idC 'y)))
					(numC 4)))
			    (numC 3))))

(test (numV 42) (interp-ast-val (unboxC (boxC (numC 42)))))

; we want to test something like this...
;(let (x (box 42))
;  (begin
;    (setbox x 43)
;    (unbox x)))
; but we don't have 'let', so desugar to lambda...
;((lambda (x) 
;   (begin
;     (setbox x 43)
;     (unbox x)))
; (box 42))
; and test	 
(test (numV 43)
      (interp-ast-val 
	(appC 
	  (lamC 'x (seqC
		     (setboxC (idC 'x) (numC 43))
		     (unboxC (idC 'x))))
	  (boxC (numC 42)))))


; test store
(test/exn (fetch 0 mt-store) "location not found")
(test (numV 42) (fetch 0 (override-store (cell 0 (numV 42)) mt-store)))
(let ([sto (override-store (cell 0 (numV 42)) mt-store)])
  (test (list (cell 0 (numV 43)))
	(override-store (cell 0 (numV 43)) sto)))

; test env
(test/exn (lookup 'x mt-env) "name not found")
(test 0 (lookup 'x (extend-env (bind 'x 0) mt-env)))
(test 1 (lookup 'x (extend-env (bind 'x 1) 
			       (extend-env (bind 'x 0) 
					   mt-env))))

; test parser/interp
; don't have syntax for box, unbox, setbox, begin, or let
; in the parser, although begin and let could be desugared
; to lambda's
(test (numC 42) 
      (parse '42))
(test (plusC (numC 1) (numC 2)) 
      (parse '(+ 1 2)))
(test (plusC (numC 1) (numC 2)) 
      (parse '(+ 1 2)))
(test (multC (plusC (numC 1) (numC 1)) 
	     (numC 2))
      (parse '(* (+ 1 1) 2)))
(test (multC (numC 1) 
	     (plusC (numC 2) (numC 2))) 
      (parse '(* 1 (+ 2 2))))
(test (lamC 'x (plusC (idC 'x) (idC 'x)))
      (parse '(lambda (x) (+ x x))))
(test (appC (lamC 'x (idC 'x)) (plusC (numC 1) (numC 1)))
      (parse '((lambda (x) x) (+ 1 1))))

;; Exercise to desugar begin to lambda's
;(begin
;  (+ 1 2)
;  (+ 3 4))
;
;; desugar to 'let'
;(let (r1 (+ 1 2))
;  (let (r2 (+ 3 4))
;    r2))
;
;; desugar to 'lambda'
;((lambda (r1)
;   (lambda (r2)
;     r2))
; (+ 1 2)
; (+ 3 4))
