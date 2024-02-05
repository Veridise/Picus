#lang racket/base

(module+ test
  (require "testlib.rkt")

  (current-solver solver:cvc5-bitsum)

  (check-result (make-run-config "circomlib-matrix-d41bae3/matAdd_test.circom") 'safe)
  (check-result (make-run-config "circomlib-matrix-d41bae3/matElemMul_test.circom") 'safe)
  (check-result (make-run-config "circomlib-matrix-d41bae3/matElemPow_test.circom") 'safe)
  (check-result (make-run-config "circomlib-matrix-d41bae3/matMul_test.circom") 'safe)
  (check-result (make-run-config "circomlib-matrix-d41bae3/matPow_test.circom") 'safe)
  (check-result (make-run-config "circomlib-matrix-d41bae3/matScalarAdd_test.circom") 'safe)
  (check-result (make-run-config "circomlib-matrix-d41bae3/matScalarMul_test.circom") 'safe)
  (check-result (make-run-config "circomlib-matrix-d41bae3/matScalarSub_test.circom") 'safe)
  (check-result (make-run-config "circomlib-matrix-d41bae3/matSub_test.circom") 'safe)
  (check-result (make-run-config "circomlib-matrix-d41bae3/outer_test.circom") 'safe)
  (check-result (make-run-config "circomlib-matrix-d41bae3/trace_test.circom") 'safe)
  (check-result (make-run-config "circomlib-matrix-d41bae3/tranpose_test.circom") 'safe))
