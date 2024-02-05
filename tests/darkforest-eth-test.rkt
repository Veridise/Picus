#lang racket/base

(module+ test
  (require "testlib.rkt")

  (current-solver solver:cvc5-bitsum)

  (check-result (make-run-config "darkforest-eth-9033eaf-fixed/init.circom") 'timeout)
  (check-result (make-run-config "darkforest-eth-9033eaf-fixed/move.circom") 'timeout)
  (check-result (make-run-config "darkforest-eth-9033eaf-fixed/reveal.circom") 'timeout)
  (check-result (make-run-config "darkforest-eth-9033eaf-fixed/test_perlin.circom") 'timeout)
  (check-result (make-run-config "darkforest-eth-9033eaf-fixed/test_range_proof.circom") 'safe)
  (check-result (make-run-config "darkforest-eth-9033eaf-fixed/whitelist.circom") 'safe))
