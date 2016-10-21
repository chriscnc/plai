#lang plai-typed

(require "interp.rkt")
(print-only-errors true)

(define (eval [x : ExprC]) : Value
  (interp x (mt-env)))

(test (numV 42) 
      (eval (numC 42)))

(test (numV 3)
      (eval (plusC (numC 1)
		   (numC 2))))

(test (numV 2)
      (eval (multC (numC 1)
		   (numC 2))))


; (+ 10 ((const5 _ 5) 10))
(test (numV 15)
      (eval (plusC (numC 10)
		   (appC (lamC '_ (numC 5))
			 (numC 10)))))

; (f1 (x) ((f2 (y) (+ x y)) 4) 3)
(test (numV 7) 
      (eval (appC (lamC 'x 
			(appC (lamC 'y 
				    (plusC (idC 'x)
					   (idC 'y)))
			      (numC 4)))
		  (numC 3))))


