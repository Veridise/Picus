#lang racket/base

(module+ test
  (require "testlib.rkt")

  (check-result (make-run-config "buggy-mix/circomlib-79d3034/test-mimcsponge.circom") 'unknown)
  (check-result (make-run-config "buggy-mix/iden3-core-3a3a300/credentialAtomicQuerySigTest.circom") 'safe)
  (check-result (make-run-config "buggy-mix/circom-ecdsa-436665b/test-bigmod22.circom") 'timeout)
  (check-result (make-run-config "buggy-mix/hermez-network-971c89f/test-rollup-main-L1.circom") 'timeout)

  (check-result (make-run-config "buggy-mix/tornado-core-ce97895/withdraw.circom") 'safe)
  (check-result (make-run-config "buggy-mix/re-tornado-core-ce97895/withdraw.circom") 'timeout)
  (check-result (make-run-config "buggy-mix/min0-tornado-core-ce97895/withdraw.circom") 'timeout))
