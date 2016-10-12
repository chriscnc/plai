#lang racket

(require rackunit
	 "parser.rkt")

(define fds 
  (list
    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))
    (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
    (fdC 'const5 '_ (numC 5))))

(check-equal? 42 (interp (parse '42) fds))
(check-equal? 3 (interp (parse '(+ 1 2)) fds))
(check-equal? 2 (interp (parse '(* 1 2)) fds))
(check-equal? 3 (interp (parse '(+ (* 1 2) 1)) fds))
(check-equal? 7 (interp (parse '(+ (* 1 2) (+ 2 3))) fds))

(check-equal? 7 (interp (parse '(+ 1 (double 3))) fds))
(check-equal? 6 (interp (parse '(+ 1 (const5 _))) fds))
(check-equal? 17 (interp (parse '(+ 1 (quadruple 4))) fds))
