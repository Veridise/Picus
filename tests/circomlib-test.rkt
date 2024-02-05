#lang racket/base

(module+ test
  (require "testlib.rkt")

  ;; Failed with cvc5-bitsum
  (check-result (make-run-config "circomlib-cff5ab6/BabyDbl@babyjub.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/MontgomeryDouble@montgomery.circom") 'unsafe)

  (check-result (make-run-config "circomlib-cff5ab6/AND@gates.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/BabyAdd@babyjub.circom") 'unknown)
  (check-result (make-run-config "circomlib-cff5ab6/BabyPbk@babyjub.circom") 'timeout)
  (check-result (make-run-config "circomlib-cff5ab6/BinSub@binsub.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/BinSum@binsum.circom") 'safe)

  ;; NOTE: was unsafe, but the basis lemma fix made it unknown (with longer time limit)
  (check-result (make-run-config "circomlib-cff5ab6/BitElementMulAny@escalarmulany.circom") 'timeout)

  (check-result (make-run-config "circomlib-cff5ab6/Bits2Num_strict@bitify.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/Bits2Num@bitify.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/Bits2Point_Strict@pointbits.circom") 'timeout)
  (check-result (make-run-config "circomlib-cff5ab6/CompConstant@compconstant.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/Decoder@multiplexer.circom") 'unsafe)
  (check-result (make-run-config "circomlib-cff5ab6/Edwards2Montgomery@montgomery.circom") 'unsafe)

  ;; NOTE: unknown (with longer time limit)
  (check-result (make-run-config "circomlib-cff5ab6/EscalarMulAny@escalarmulany.circom") 'timeout)

  (check-result (make-run-config "circomlib-cff5ab6/EscalarProduct@multiplexer.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/GreaterEqThan@comparators.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/GreaterThan@comparators.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/IsEqual@comparators.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/IsZero@comparators.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/LessEqThan@comparators.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/LessThan@comparators.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/MiMC7@mimc.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/MiMCFeistel@mimcsponge.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/MiMCSponge@mimcsponge.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/Montgomery2Edwards@montgomery.circom") 'unsafe)
  (check-result (make-run-config "circomlib-cff5ab6/MontgomeryAdd@montgomery.circom") 'unsafe)
  (check-result (make-run-config "circomlib-cff5ab6/MultiAND@gates.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/MultiMiMC7@mimc.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/MultiMux1@mux1.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/MultiMux2@mux2.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/MultiMux3@mux3.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/MultiMux4@mux4.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/Multiplexer@multiplexer.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/Multiplexor2@escalarmulany.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/Mux1@mux1.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/Mux2@mux2.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/Mux3@mux3.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/Mux4@mux4.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/NAND@gates.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/NOR@gates.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/NOT@gates.circom") 'safe)

  ;; was safe, but the basis lemma fix made it timed out
  (check-result (make-run-config "circomlib-cff5ab6/Num2Bits_strict@bitify.circom") 'timeout)

  (check-result (make-run-config "circomlib-cff5ab6/Num2Bits@bitify.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/Num2BitsNeg@bitify.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/OR@gates.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/Pedersen@pedersen_old.circom") 'safe)

  ;; NOTE: unknown (with longer time limit)
  (check-result (make-run-config "circomlib-cff5ab6/Pedersen@pedersen.circom") 'timeout)

  ;; was safe, but the basis lemma fix made it timed out
  (check-result (make-run-config "circomlib-cff5ab6/Point2Bits_Strict@pointbits.circom") 'timeout)

  (check-result (make-run-config "circomlib-cff5ab6/Poseidon@poseidon.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/Segment@pedersen.circom") 'timeout)
  (check-result (make-run-config "circomlib-cff5ab6/SegmentMulAny@escalarmulany.circom") 'timeout)
  (check-result (make-run-config "circomlib-cff5ab6/SegmentMulFix@escalarmulfix.circom") 'timeout)
  (check-result (make-run-config "circomlib-cff5ab6/Sigma@poseidon.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/Sign@sign.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/Switcher@switcher.circom") 'safe)
  (check-result (make-run-config "circomlib-cff5ab6/Window4@pedersen.circom") 'timeout)
  (check-result (make-run-config "circomlib-cff5ab6/WindowMulFix@escalarmulfix.circom") 'timeout)
  (check-result (make-run-config "circomlib-cff5ab6/XOR@gates.circom") 'safe))
