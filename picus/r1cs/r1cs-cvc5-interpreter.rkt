#lang racket
; this interprets r1cs commands into cvc5 constraints
(require (prefix-in config: "../config.rkt")
         (prefix-in r1cs: "./r1cs-grammar.rkt")
         "common.rkt")
(provide interpret-r1cs)

(define (interpret-r1cs arg-r1cs)
  (interp
   arg-r1cs
   (λ (arg-r1cs fallback)
     (let loop ([arg-r1cs arg-r1cs])
       (match arg-r1cs
         [(r1cs:rint v) (printf "#f~am~a" v config:p)]
         [(r1cs:radd vs) (format-op "ff.add" loop vs)]
         [(r1cs:rmul vs) (format-op "ff.mul" loop vs)]
         [_ (fallback arg-r1cs)])))))
