#lang plai-typed

(define (read-number [prompt : string]) : number
  (begin
    (display prompt)
    (let ([v (read)])
      (if (s-exp-number? v)
          (s-exp->number v)
          (read-number prompt)))))

(define-type-alias label number)

(define new-label
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

;; server-side state
(define table (make-hash empty))

(define (read-number/suspend [prompt : string] rest)
  (let ([g (new-label)])
    (begin
      (hash-set! table g rest)
      (display prompt)
      (display " To enter it, use the action field label ")
      (display g)
      (display "\nhalting: Program shut down\n"))))

(define (resume [g : label] [n : number])
  ((some-v (hash-ref table g)) n))

(define cookie '-100)

;; server prompts sends page for user to enter first number
;; and immediately terminates, putting the continuation in
;; the hash-table with label 1
(read-number/suspend "First number"
                     (lambda (v1)
                       (begin
                         (set! cookie v1)
                         (read-number/suspend "Second number"
                                              (lambda (v2)
                                                (display
                                                 (+ cookie v2)))))))

;; User submits 3 as the first number. The label 1 is passed
;; along so the server can retrieve the continuation
(resume 1 3)

;; which will produce a new page to prompt for the second number
;; storing the contination in the hash-table at label 2

;; User submits 10 as the second number.  The label 2 is passed
;; along to the server which uses it to look up the continuation
;; and run it 
(resume 2 10)

;; producing the answer.
