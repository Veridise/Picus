#lang racket/base

(module+ test
  (require racket/sandbox
           (prefix-in r1cs: "../picus/r1cs/r1cs-grammar.rkt"))

  ;; read this within 30s or error
  (with-limits 30 #f
    (void (r1cs:read-r1cs "../benchmarks/r1cs/TreeHasher.r1cs"))))
