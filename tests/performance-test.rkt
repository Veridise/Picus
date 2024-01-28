#lang racket/base

(module+ test
  (require racket/sandbox
           "../picus/reader.rkt"
           (prefix-in cvc5-parser: "../picus/r1cs/r1cs-cvc5-parser.rkt")
           (prefix-in z3-parser: "../picus/r1cs/r1cs-z3-parser.rkt"))

  ;; read this within 20s or error
  (define r0
    (with-limits 20 #f
      ((r1cs "../benchmarks/r1cs/TreeHasher.r1cs") #:opt-level #f)))

  ;; generate constraints within 20s or error
  (with-limits 20 #f
    (call-with-values (λ () (cvc5-parser:parse-r1cs r0 '())) void))

  (with-limits 20 #f
    (call-with-values (λ () (z3-parser:parse-r1cs r0 '())) void)))
