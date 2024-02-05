#lang racket/base

(module+ test
  (require "testlib.rkt")

  (current-solver solver:cvc5-bitsum)

  (check-result (make-run-config "circom-ecdsa-d87eb70/ecdsa.circom") 'timeout)
  (check-result (make-run-config "circom-ecdsa-d87eb70/ecdsa_verify.circom") 'timeout)
  (check-result (make-run-config "circom-ecdsa-d87eb70/secp256k1_add.circom") 'timeout)
  (check-result (make-run-config "circom-ecdsa-d87eb70/secp256k1_double.circom") 'timeout)
  (check-result (make-run-config "circom-ecdsa-d87eb70/secp256k1_poc.circom") 'safe)
  (check-result (make-run-config "circom-ecdsa-d87eb70/secp256k1_scalarmult.circom") 'timeout))
