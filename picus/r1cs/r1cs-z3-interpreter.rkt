#lang racket
; this interprets r1cs commands into z3 constraints
(require (prefix-in r1cs: "./r1cs-grammar.rkt")
         "common.rkt")

(provide interpret-r1cs)

(define (interpret-r1cs arg-r1cs)
  (interp
   arg-r1cs
   (λ (arg-r1cs fallback)
     (let loop ([arg-r1cs arg-r1cs])
       (match arg-r1cs
         [(r1cs:rleq lhs rhs) (emit "(<= " (loop lhs) " " (loop rhs) ")")]
         [(r1cs:rlt lhs rhs) (emit "(< " (loop lhs) " " (loop rhs) ")")]
         [(r1cs:rgeq lhs rhs) (emit "(>= " (loop lhs) " " (loop rhs) ")")]
         [(r1cs:rgt lhs rhs) (emit "(> " (loop lhs) " " (loop rhs) ")")]

         [(r1cs:rint v) (display v)]

         [(r1cs:radd vs) (format-op "+" loop vs)]
         [(r1cs:rsub vs) (format-op "-" loop vs)]
         [(r1cs:rmul vs) (format-op "*" loop vs)]
         [(r1cs:rneg v) (emit "(- " (loop v) ")")]
         [(r1cs:rmod v mod) (emit "(rem " (loop v) " " (loop mod) ")")]
         [_ (fallback arg-r1cs)])))))
