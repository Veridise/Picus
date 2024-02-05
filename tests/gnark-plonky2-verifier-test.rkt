#lang racket/base

(module+ test
  (require "testlib.rkt")

  (check-result (make-run-config "gnark-plonky2-verifier/pure/inverse.timeout.sr1cs") 'timeout)
  (check-result (make-run-config "gnark-plonky2-verifier/pure/mul-add.timeout.sr1cs") 'timeout)
  (check-result (make-run-config "gnark-plonky2-verifier/pure/reduce.timeout.sr1cs") 'timeout)

  (parameterize ([current-solver solver:cvc5-int])
    (check-result (make-run-config "gnark-plonky2-verifier/int/exp.safe.sr1cs") 'safe)
    (check-result (make-run-config "gnark-plonky2-verifier/int/inverse.unsafe.sr1cs") 'unsafe)
    (check-result (make-run-config "gnark-plonky2-verifier/int/mul-add.safe.sr1cs") 'safe)
    (check-result (make-run-config "gnark-plonky2-verifier/int/reduce.unsafe.sr1cs") 'timeout) ; actually unsafe

    (check-result (make-run-config "gnark-plonky2-verifier/fixed-int/inverse.unknown.sr1cs") 'unknown)
    (check-result (make-run-config "gnark-plonky2-verifier/fixed-int/reduce.timeout.sr1cs") 'timeout)))
