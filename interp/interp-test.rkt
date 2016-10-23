#lang plai-typed

(require "interp.rkt")
(print-only-errors true)

(define (interp-ast [x : ExprC]) : Value
  (interp x (mt-env)))

(test (numV 42) 
      (interp-ast (numC 42)))

(test (numV 3)
      (interp-ast (plusC (numC 1)
			 (numC 2))))

(test (numV 2)
      (interp-ast (multC (numC 1)
			 (numC 2))))


; (+ 10 ((lambda (_) 5) 10))
(test (numV 15)
      (interp-ast (plusC (numC 10)
			 (appC (lamC '_ (numC 5))
			       (numC 10)))))

; (lambda (x) ((lambda (y) (+ x y)) 4) 3)
(test (numV 7) 
      (interp-ast (appC (lamC 'x 
			      (appC (lamC 'y 
					  (plusC (idC 'x)
						 (idC 'y)))
				    (numC 4)))
			(numC 3))))

(define (eval [expr : s-expression]) : Value
  (interp (parse expr) (mt-env)))

(test (numV 42) (eval '42))
(test (numV 3) (eval '(+ 1 2)))
(test (numV 2) (eval '(* 1 2)))
(test (closV 'x (idC 'x) (mt-env)) (eval '(lambda (x) x)))
;(test (numV 15) (eval '(+ 10 ((lambda (_) 5) 10))))
(parse '(lambda x 5))
(parse '(+ 10 ((lambda (x) 5) 10)))
