#lang plai-typed

(require "typed-lang.rkt")

(define (make-ids (n : number)) : (listof symbol)
  (build-list n (lambda (n) (string->symbol (string-append "var-" (to-string n))))))

;; cascade-lets will build up the nested lets, and use body as the
;; eventual body, preserving order of evaluation of the expressions
(define (cascade-lets (ids : (listof symbol))
                      (exprs : (listof ExprC))
                      (body : ExprC)) : ExprC
  (cond [(empty? ids) body]
        [(cons? ids)
         (LetC (first ids) (first exprs) (cascade-lets (rest ids) (rest exprs) body))]))

;; check-type builds an expression that checks the type of the expression
;; given as an argument
(define (check-type (expr : ExprC) (type : string)) : ExprC
  (Prim2C '== (Prim1C 'tagof expr) (StrC type)))

;; and builds up an and expression from its two pieces
(define (and (expr1 : ExprC) (expr2 : ExprC)) : ExprC
  (IfC expr1 expr2 (FalseC)))

;; all builds up a series of ands over the expression arguments
(define (all (exprs : (listof ExprC))) : ExprC
  (foldl (lambda (exp result) (and exp result)) (TrueC) exprs))

;; map-subtract builds an expression that maps 'num- over a list of expressions
(define (map-subtract (exprs : (listof ExprC))) : ExprC
  (foldl (lambda (expr result) (Prim2C 'num- result expr)) (first exprs) (rest exprs)))

;; map-add-num builds an expression that maps 'num+ over a list of expressions
(define (map-add-num (exprs : (listof ExprC))) : ExprC
  (foldl (lambda (expr result) (Prim2C 'num+ result expr)) (first exprs) (rest exprs)))

;; map-add-str builds an expression that maps 'string+ over a list of expressions
(define (map-add-str (exprs : (listof ExprC))) : ExprC
  (foldl (lambda (expr result) (Prim2C 'string+ result expr)) (first exprs) (rest exprs)))

(define (desugar-seq (es : (listof ExprP))): ExprC
  (let ([desugared-es (map desugar es)])
    (foldl (lambda (expr result) (SeqC result expr))
	   (first desugared-es)
	   (rest desugared-es))))

(define (desugar-subtract (args : (listof ExprP))) : ExprC
  (local ([define ids (make-ids (length args))]
          [define id-exps (map IdC ids)])
    (cascade-lets ids (map desugar args)
      (IfC (all (map (lambda (e) (check-type e "number")) id-exps))
           (map-subtract id-exps)
           (ErrorC (StrC "Bad arguments to -"))))))

(define (desugar-add (args : (listof ExprP))) : ExprC
  (local ([define ids (make-ids (length args))]
	  [define id-exps (map IdC ids)])
    (cascade-lets ids (map desugar args)
      (IfC (all (map (lambda (e) (check-type e "number")) id-exps))
	   (map-add-num id-exps)
	   (IfC (all (map (lambda (e) (check-type e "string")) id-exps))
		(map-add-str id-exps)
		(ErrorC (StrC "Bad arguments to -")))))))

(define (desugar-gt (args : (listof ExprP))) : ExprC
  (let ([a1 (desugar (first args))]
	[a2 (desugar (second args))])
    (IfC (and (check-type a1 "number")
	      (check-type a2 "number"))
	 (Prim2C '> a1 a2)
	 (ErrorC (StrC "Bad arguments to >")))))

(define (desugar-lt (args : (listof ExprP))) : ExprC
  (let ([a1 (desugar (first args))]
	[a2 (desugar (second args))])
    (IfC (and (check-type a1 "number")
	      (check-type a2 "number"))
	 (Prim2C '< a1 a2)
	 (ErrorC (StrC "Bad arguments to <")))))

(define (desugar (exprP : ExprP)) : ExprC
  (type-case ExprP exprP

    [NumP (n) (NumC n)]
    ;; Fill in more cases here...
    [StrP (s) (StrC s)]
    [TrueP () (TrueC)]
    [FalseP () (FalseC)]
    [IdP (name) (IdC name)]
    [IfP (c t e) (IfC (desugar c) (desugar t) (desugar e))]

    [PrimP (op args)
        (case op
          ['- (cond
                [(= 0 (length args)) (ErrorC (StrC "Empty list for prim op"))]
                [(< 0 (length args)) (desugar-subtract args)])]
	  ['+ (cond
		[(= 0 (length args)) (ErrorC (StrC "Empty list for prim op"))]
		[(< 0 (length args)) (desugar-add args)])]
	  ['== (cond
		 [(= 0 (length args)) (ErrorC (StrC "Empty list for prim op"))]
		 [(not (= 2 (length args))) (ErrorC (StrC "Bad primop"))]
		 [else (Prim2C '== 
			       (desugar (first args))
			       (desugar (second args)))])]
	  ['< (cond
		 [(= 0 (length args)) (ErrorC (StrC "Empty list for prim op"))]
		 [(not (= 2 (length args))) (ErrorC (StrC "Bad primop"))]
		 [else (desugar-lt args)])]
	  ['> (cond
		 [(= 0 (length args)) (ErrorC (StrC "Empty list for prim op"))]
		 [(not (= 2 (length args))) (ErrorC (StrC "Bad primop"))]
		 [else (desugar-gt args)])]
	  ['print (cond
		    [(= 0 (length args)) (ErrorC (StrC "Empty list for prim op"))]
		    [(not (= 1 (length args))) (ErrorC (StrC "Bad primop"))]
		    [else (Prim1C 'print (desugar (first args)))])]
	  )]
    [SeqP (es) (desugar-seq es)]
		 


    [WhileP (test body)
          ;; dummy-fun will tell us it was called if we do so accidentally
          (local ([define dummy-fun (FuncC (list) (ErrorC (StrC "Dummy function")))])
          (IfC (desugar test)

               ;; while-var will hold the actual function once we tie
               ;; everything together
               (LetC 'while-var dummy-fun
                 (LetC 'while-func

                   ;; this function does the real work - it runs the body of
                   ;; the while loop, then re-runs it if the test is true, and
                   ;; stops if its false
                   (FuncC (list)
                     (LetC 'temp-var
                       (desugar body)
                       (IfC (desugar test)
                            (AppC (IdC 'while-var) (list))
                            (IdC 'temp-var))))

                   ;; The Set!C here makes sure that 'while-var will resolve
                   ;; to the right value later, and the AppC kicks things off
                   (SeqC (Set!C 'while-var (IdC 'while-func))
                         (AppC (IdC 'while-var) (list)))))

               (FalseC)))]
    [else (ErrorC (StrC (string-append "Haven't desugared a case yet:\n"
                                       (to-string exprP))))]))

