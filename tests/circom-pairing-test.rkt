#lang racket/base

(module+ test
  (require "testlib.rkt")

  (current-solver solver:cvc5-bitsum)

  (check-result (make-run-config "circom-pairing-743d761/bls12-381_add.circom") 'timeout)
  (check-result (make-run-config "circom-pairing-743d761/bls12-381_double.circom") 'timeout)
  (check-result (make-run-config "circom-pairing-743d761/fp12_add_22.circom") 'timeout)
  (check-result (make-run-config "circom-pairing-743d761/fp12_compression_32.circom") 'timeout)
  (check-result (make-run-config "circom-pairing-743d761/fp12_cyclotomicExp_32.circom") 'timeout)
  (check-result (make-run-config "circom-pairing-743d761/fp12_cyclotomicSquare_32.circom") 'timeout)
  (check-result (make-run-config "circom-pairing-743d761/fp12_invert_42.circom") 'timeout)
  (check-result (make-run-config "circom-pairing-743d761/fp12_multiply_32.circom") 'timeout)
  (check-result (make-run-config "circom-pairing-743d761/fp2_add_22.circom") 'timeout)
  (check-result (make-run-config "circom-pairing-743d761/fp2_invert_42.circom") 'timeout)
  (check-result (make-run-config "circom-pairing-743d761/fp2_multiply_42.circom") 'timeout)
  (check-result (make-run-config "circom-pairing-743d761/linefunc_equal_32.circom") 'timeout)
  (check-result (make-run-config "circom-pairing-743d761/linefunc_unequal_32.circom") 'timeout)
  (check-result (make-run-config "circom-pairing-743d761/multiply_linefunc_unequal_32.circom") 'timeout)
  (check-result (make-run-config "circom-pairing-743d761/secp256k1_add.circom") 'timeout))
