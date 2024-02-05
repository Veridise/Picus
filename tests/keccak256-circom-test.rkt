#lang racket/base

(module+ test
  (require "testlib.rkt")

  (current-solver solver:cvc5-bitsum)

  (check-result (make-run-config "keccak256-circom-af3e898/absorb_test.circom") 'timeout)

  (check-result (make-run-config "keccak256-circom-af3e898/chi_test.circom") 'safe)
  (check-result (make-run-config "keccak256-circom-af3e898/final_test.circom") 'timeout)
  (check-result (make-run-config "keccak256-circom-af3e898/iota3_test.circom") 'safe)
  (check-result (make-run-config "keccak256-circom-af3e898/keccak_32_256_test.circom") 'timeout)
  (check-result (make-run-config "keccak256-circom-af3e898/keccakfRound0_test.circom") 'safe)
  (check-result (make-run-config "keccak256-circom-af3e898/keccakf_test.circom") 'timeout)
  (check-result (make-run-config "keccak256-circom-af3e898/pad_test.circom") 'safe)
  (check-result (make-run-config "keccak256-circom-af3e898/rhopi_test.circom") 'safe)
  (check-result (make-run-config "keccak256-circom-af3e898/squeeze_test.circom") 'safe)
  (check-result (make-run-config "keccak256-circom-af3e898/theta_test.circom") 'safe))
