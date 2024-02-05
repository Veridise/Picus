#lang racket/base

(module+ test
  (require "testlib.rkt")

  (current-solver solver:cvc5-bitsum)

  (check-result (make-run-config "circomlibex-cff5ab6/T1@t1@circomlib.circom") 'timeout)
  (check-result (make-run-config "circomlibex-cff5ab6/T2@t2@circomlib.circom") 'timeout)
  (check-result (make-run-config "circomlibex-cff5ab6/Switcher@switcher@circomlib.circom") 'safe)
  (check-result (make-run-config "circomlibex-cff5ab6/SigmaPlus@sigmaplus@circomlib.circom") 'timeout)
  (check-result (make-run-config "circomlibex-cff5ab6/Sigma@poseidon_old@circomlib.circom") 'safe)
  (check-result (make-run-config "circomlibex-cff5ab6/Sha256compression@sha256compression@circomlib.circom") 'timeout)
  (check-result (make-run-config "circomlibex-cff5ab6/Sha256_2@sha256_2@circomlib.circom") 'timeout)
  (check-result (make-run-config "circomlibex-cff5ab6/SMTVerifierSM@smtverifiersm@circomlib.circom") 'safe)
  (check-result (make-run-config "circomlibex-cff5ab6/SMTVerifierLevel@smtverifierlevel@circomlib.circom") 'safe)
  (check-result (make-run-config "circomlibex-cff5ab6/SMTProcessorSM@smtprocessorsm@circomlib.circom") 'safe)
  (check-result (make-run-config "circomlibex-cff5ab6/SMTProcessorLevel@smtprocessorlevel@circomlib.circom") 'safe)
  (check-result (make-run-config "circomlibex-cff5ab6/SMTHash2@smthash_poseidon@circomlib.circom") 'safe)
  (check-result (make-run-config "circomlibex-cff5ab6/SMTHash2@smthash_mimc@circomlib.circom") 'safe)
  (check-result (make-run-config "circomlibex-cff5ab6/SMTHash1@smthash_poseidon@circomlib.circom") 'safe)
  (check-result (make-run-config "circomlibex-cff5ab6/SMTHash1@smthash_mimc@circomlib.circom") 'safe)
  (check-result (make-run-config "circomlibex-cff5ab6/ForceEqualIfEnabled@comparators@circomlib.circom") 'safe)
  (check-result (make-run-config "circomlibex-cff5ab6/EdDSAPoseidonVerifier@eddsaposeidon@circomlib.circom") 'safe)
  (check-result (make-run-config "circomlibex-cff5ab6/EdDSAMiMCVerifier@eddsamimc@circomlib.circom") 'safe)
  (check-result (make-run-config "circomlibex-cff5ab6/EdDSAMiMCSpongeVerifier@eddsamimcsponge@circomlib.circom") 'safe))
