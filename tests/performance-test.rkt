#lang racket/base

(module+ test
  (require rackunit
           racket/sandbox
           "../picus/reader.rkt"
           (prefix-in cvc5-parser: "../picus/r1cs/r1cs-cvc5-parser.rkt")
           (prefix-in z3-parser: "../picus/r1cs/r1cs-z3-parser.rkt"))

  ;; read this within 20s or error
  (define r0
    (with-limits 20 #f
      (begin0 ((r1cs "../benchmarks/r1cs/TreeHasher.r1cs") #:opt-level #f)
        (check-true #t "read r0"))))

  ;; generate constraints within 20s or error
  (with-limits 20 #f
    (cvc5-parser:parse-r1cs r0)
    (check-true #t "successfully converted to cvc5 format"))

  (with-limits 20 #f
    (z3-parser:parse-r1cs r0)
    (check-true #t "successfully converted to z3 format")))
