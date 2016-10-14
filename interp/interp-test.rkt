#lang plai-typed

(require "interp.rkt")
(print-only-errors true)

(define fds 
  (list
    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))
    (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
    (fdC 'const5 '_ (numC 5))
    (fdC 'f1 'x (appC 'f2 (numC 4)))
    (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))))

(test 42 (interp (parse '42) mt-env fds))
(test 3 (interp (parse '(+ 1 2)) mt-env fds))
(test 2 (interp (parse '(* 1 2)) mt-env fds))
(test 3 (interp (parse '(+ (* 1 2) 1)) mt-env fds))
(test 7 (interp (parse '(+ (* 1 2) (+ 2 3))) mt-env fds))

(test 15 (interp (parse '(+ 10 (const5 10)))  mt-env fds))
(test 16 (interp (parse '(+ 10 (double (+ 1 2)))) mt-env fds))
(test 22 (interp (parse '(+ 10 (quadruple (+ 1 2))))  mt-env fds))
; free variables should cause an error
(test/exn (interp (parse '(f1 3)) mt-env fds) "name not found")

