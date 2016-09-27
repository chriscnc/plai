#lang racket

(require rackunit
	 "parser.rkt")

(check-equal? 42 (interp (parse '42)))
(check-equal? 3 (interp (parse '(+ 1 2))))
(check-equal? 2 (interp (parse '(* 1 2))))
(check-equal? 3 (interp (parse '(+ (* 1 2) 1))))
(check-equal? 7 (interp (parse '(+ (* 1 2) (+ 2 3)))))

