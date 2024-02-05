#lang racket/base

(module+ test
  (require "testlib.rkt")

  ;; Failed with cvc5-bitsum
  (check-result (make-run-config "circom-bigint-7505e5c/test_bigmult_21.circom") 'safe)

  (check-result (make-run-config "circom-bigint-7505e5c/test_bigadd_15.circom") 'safe)
  (check-result (make-run-config "circom-bigint-7505e5c/test_bigadd_23.circom") 'safe)
  (check-result (make-run-config "circom-bigint-7505e5c/test_biglessthan_23.circom") 'safe)
  (check-result (make-run-config "circom-bigint-7505e5c/test_bigmod_22.circom") 'timeout)
  ;; (check-result (make-run-config "circom-bigint-7505e5c/test_bigmod_32") 'timeout)
  (check-result (make-run-config "circom-bigint-7505e5c/test_bigmult_22.circom") 'timeout)
  ;; (check-result (make-run-config "circom-bigint-7505e5c/test_bigmult_23") 'timeout)
  (check-result (make-run-config "circom-bigint-7505e5c/test_bigsub_15.circom") 'safe)
  (check-result (make-run-config "circom-bigint-7505e5c/test_bigsub_23.circom") 'safe)
  (check-result (make-run-config "circom-bigint-7505e5c/test_bigsubmodp_32.circom") 'safe))
