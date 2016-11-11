#lang plai-typed

(require "interp.rkt")
(print-only-errors true)

(define (tce [x : ExprC]) : Type
  (tc x mt-env))

(test (numT) 
      (tce (numC 42)))

(test (numT)
      (tce (plusC (numC 1)
		  (numC 2))))

(test (numT)
      (tce (multC (numC 1)
		  (numC 2))))

(test/exn (tce (multC (lamC 'x (numT) (numT) (numC 1))
		      (numC 2)))
	  "* type error")

(test/exn (tce (plusC (lamC 'x (numT) (numT) (numC 1))
		      (numC 2)))
	  "+ type error")

(test (numT)
      (tce (ifC (boolC true)
		(numC 42)
		(numC 24))))

(test/exn (tce (ifC (boolC true)
		    (boolC true)
		    (numC 24)))
	  "if then and else not same type")

(test/exn (tce (ifC (numC 42)
		    (numC 24)
		    (numC 24)))
	  "if condition not boolean")

(test/exn (tce (if0C (boolC true)
		     (numC 24)
		     (numC 24)))
	  "if0 condition not number")

(test/exn (tce (if0C (numC 0)
		     (boolC true)
		     (numC 24)))
	  "if0 then and else not same type")

; something a little more complicated
(test/exn (tce (ifC (appC (lamC 'x (numT) (numT) (plusC (idC 'x)
							(idC 'x)))
			  (numC 10))
		    (numC 24)
		    (numC 24)))
	  "if condition not boolean")

(test (numT)
      (tce (recC 'fact 'n (numT) (numT)
		 (if0C (idC 'n)
		       (numC 0)
		       (multC (idC 'n) 
			      (appC (idC 'fact)
				    (plusC (idC 'n) (numC -1)))))
		 (appC (idC 'fact) (numC 5)))))


(test (funT (numT) (numT)) 
      (tce (lamC 'x 
		 (numT) 
		 (numT) 
		 (plusC (idC 'x)
			(idC 'x)))))

(test (numT)
      (tce (appC (lamC 'x (numT) (numT) (plusC (idC 'x)
					       (idC 'x)))
		 (numC 10))))
			
(test/exn (tce (appC (numC 1) (numC 10)))
	  "non function type in function position")


(test/exn (tce (appC (lamC 'x 
			   (numT) 
			   (numT) 
			   (plusC (idC 'x) (idC 'x)))
		 (lamC 'y (numT) (numT) (numC 10))))
	  "app arg type mismatch")


(test/exn (tce (lamC 'x
		 (numT) 
		 (numT) 
		 (lamC 'y (numT) (numT) (numC 42))))
      "lam type mismatch")

; test env
(test/exn (lookup 'x mt-env) "name not found")
(test (numT) (lookup 'x (extend-env (bind 'x (numT)) mt-env)))

