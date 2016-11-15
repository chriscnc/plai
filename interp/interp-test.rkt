#lang plai-typed

(require "interp.rkt")
(print-only-errors true)


;(test (numT) 
;      (cg (numC 42))
;
;(test (numT)
;      (tce (plusC (numC 1)
;		  (numC 2))))
;
;(test (numT)
;      (tce (multC (numC 1)
;		  (numC 2))))
;
;(test/exn (tce (multC (lamC 'x (numT) (numT) (numC 1))
;		      (numC 2)))
;	  "* type error")
;
;(test/exn (tce (plusC (lamC 'x (numT) (numT) (numC 1))
;		      (numC 2)))
;	  "+ type error")
;
;
;(test (funT (numT) (numT)) 
;      (tce (lamC 'x 
;		 (numT) 
;		 (numT) 
;		 (plusC (idC 'x)
;			(idC 'x)))))
;
;(test (numT)
;      (tce (appC (lamC 'x (numT) (numT) (plusC (idC 'x)
;					       (idC 'x)))
;		 (numC 10))))
;			
;(test/exn (tce (appC (numC 1) (numC 10)))
;	  "non function type in function position")
;
;
;(test/exn 
(cg (appC (lamC 'x 
		(plusC (idC 'x) (idC 'x)))
	  (lamC 'y (numC 10))))
;	  "app arg type mismatch")
;
;
;(test/exn (tce (lamC 'x
;		 (numT) 
;		 (numT) 
;		 (lamC 'y (numT) (numT) (numC 42))))
;      "lam type mismatch")
;
;; test env
;(test/exn (lookup 'x mt-env) "name not found")
;(test (numT) (lookup 'x (extend-env (bind 'x (numT)) mt-env)))
;
