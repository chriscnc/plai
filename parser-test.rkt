#lang plai-typed
;racket

;require rackunit
(require "parser.rkt")

(test 42 (interp (desugar (parse '42))))
(test -42 (interp (desugar (parse '-42))))
(test 3 (interp (desugar (parse '(+ 1 2)))))
(test 3.3 (interp (desugar (parse '(+ 1.1 2.2)))))
(test 2 (interp (desugar (parse '(* 1 2)))))
(test 3 (interp (desugar (parse '(+ (* 1 2) 1)))))
(test 7 (interp (desugar (parse '(+ (* 1 2) (+ 2 3))))))


