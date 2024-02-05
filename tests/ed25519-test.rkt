#lang racket/base

(module+ test
  (require "testlib.rkt")

  (current-solver solver:cvc5-bitsum)

  ;; Succeesfully solved with cvc5-bitsum
  (check-result (make-run-config "ed25519-099d19c-fixed/chunkedadd.circom") 'unsafe)
  (check-result (make-run-config "ed25519-099d19c-fixed/binmulfast51_2.circom") 'unsafe)
  (check-result (make-run-config "ed25519-099d19c-fixed/binadd1.circom") 'unsafe)

  (check-result (make-run-config "ed25519-099d19c-fixed/batchverify.circom") 'timeout)
  (check-result (make-run-config "ed25519-099d19c-fixed/binaddirr.circom") 'timeout)
  (check-result (make-run-config "ed25519-099d19c-fixed/binmul1.circom") 'timeout)
  (check-result (make-run-config "ed25519-099d19c-fixed/binmullessthan51.circom") 'unsafe)
  (check-result (make-run-config "ed25519-099d19c-fixed/binmulfast1.circom") 'timeout)
  (check-result (make-run-config "ed25519-099d19c-fixed/binsub1.circom") 'timeout)
  (check-result (make-run-config "ed25519-099d19c-fixed/chunkedmodulus.circom") 'timeout)
  (check-result (make-run-config "ed25519-099d19c-fixed/chunkify1.circom") 'safe)
  (check-result (make-run-config "ed25519-099d19c-fixed/inversemodulo1.circom") 'timeout)
  (check-result (make-run-config "ed25519-099d19c-fixed/modinv.circom") 'timeout)
  (check-result (make-run-config "ed25519-099d19c-fixed/modulus0.circom") 'timeout)
  (check-result (make-run-config "ed25519-099d19c-fixed/modulus1.circom") 'safe)
  (check-result (make-run-config "ed25519-099d19c-fixed/modulusagainst2p.circom") 'timeout)
  (check-result (make-run-config "ed25519-099d19c-fixed/modulusq1.circom") 'safe)
  (check-result (make-run-config "ed25519-099d19c-fixed/point-addition51.circom") 'timeout)
  (check-result (make-run-config "ed25519-099d19c-fixed/pointcompress.circom") 'timeout)
  (check-result (make-run-config "ed25519-099d19c-fixed/scalarmul.circom") 'timeout)
  (check-result (make-run-config "ed25519-099d19c-fixed/verify.circom") 'timeout))
