#lang racket/base

(module+ test
  (require "testlib.rkt")

  ;; Failed with cvc5-bitsum
  (check-result (make-run-config "hermez-network-9a696e3-fixed/test-rollup-tx-states.circom") 'safe)

  (check-result (make-run-config "hermez-network-9a696e3-fixed/test-hash-inputs.circom") 'timeout)

  (check-result (make-run-config "hermez-network-9a696e3-fixed/test-rollup-main-L1.circom") 'timeout)
  (check-result (make-run-config "hermez-network-9a696e3-fixed/test-balance-updater.circom") 'unknown)
  (check-result (make-run-config "hermez-network-9a696e3-fixed/test-compute-fee.circom") 'safe)
  (check-result (make-run-config "hermez-network-9a696e3-fixed/test-decode-tx.circom") 'timeout)
  (check-result (make-run-config "hermez-network-9a696e3-fixed/test-fee-accumulator.circom") 'timeout)
  (check-result (make-run-config "hermez-network-9a696e3-fixed/test-fee-tx.circom") 'timeout)
  (check-result (make-run-config "hermez-network-9a696e3-fixed/test-rollup-tx.circom") 'timeout)
  (check-result (make-run-config "hermez-network-9a696e3-fixed/test-rq-tx-verifier.circom") 'safe)
  (check-result (make-run-config "hermez-network-9a696e3-fixed/test-withdraw.circom") 'timeout))
