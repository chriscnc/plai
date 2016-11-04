#lang plai-typed

(require "typed-lang2.rkt")

(define-type Cell
  [cell (location : Location) (value : ValueC)])

(define-type-alias Store (listof Cell))

(define-type Result
  [v*s (value : ValueC) (store : Store)])

;(define-type ValueC
;  [ObjectV (fields : (listof FieldV))]
;  [ClosureV (args : (listof symbol)) (body : ExprC) (env : Env)]
;  [NumV (n : number)]
;  [StrV (s : string)]
;  [TrueV]
;  [FalseV])

(define (interp-full [exprC : ExprC] [env : Env] [store : Store]) : Result
  (type-case ExprC exprC
    [NumC (n) (v*s (NumV n) store)]
    [StrC (s) (v*s (StrV s) store)]
    [TrueC () (v*s (TrueV) store)]
    [FalseC () (v*s (FalseV) store)]
;
;    [ObjectC (fields : (listof FieldC))]
;    [GetFieldC (obj : ExprC) (field : ExprC)]
;    [SetFieldC (obj : ExprC) (field : ExprC) (value : ExprC)]
;
    [FuncC (args body) (v*s (ClosureV args body env) store)]

;    [AppC (func : ExprC) (args : (listof ExprC))]
;    [LetC (id : symbol) (bind : ExprC) (body : ExprC)]
;    [IdC (id : symbol)]
;    [Set!C (id : symbol) (value : ExprC)]
;
;    [IfC (cond : ExprC) (then : ExprC) (else : ExprC)]
    [SeqC (e1 e2) (type-case Result (interp-full e1 env store)
		    [v*s (v-e1 s-e1) 
			 (interp-full e2 env s-e1)])]
;
;
;    [ErrorC (expr : ExprC)]
;
;    ; The core operations are 'string+ 'num+ 'num- '== '< '> 'print 'tagof
;    [Prim1C (op : symbol) (arg : ExprC)]
;    [Prim2C (op : symbol) (arg1 : ExprC) (arg2 : ExprC)])
    [else (interp-error (string-append "Haven't covered a case yet:"
                                       (to-string exprC)))]))

(define (interp [exprC : ExprC]) : ValueC
  (type-case Result (interp-full exprC empty empty)
    [v*s (value store) value]))

