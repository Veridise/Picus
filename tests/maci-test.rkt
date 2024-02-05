#lang racket/base

(module+ test
  (require "testlib.rkt")

  ;; These tests have other variants as well.
  (check-result (make-run-config "maci-9b1b1a6-fixed/batchUpdateStateTree_test.circom" #:timeout 180) 'safe)
  (check-result (make-run-config "maci-9b1b1a6-fixed/quadVoteTally_test.circom") 'timeout)

  ;; Potentially unsafe?
  (check-result (make-run-config "maci-9b1b1a6-fixed/quinGeneratePathIndices_test.circom") 'timeout)

  ;; Failed with cvc5-bitsum
  (check-result (make-run-config "maci-9b1b1a6-fixed/quinSelector_test.circom") 'safe)
  (check-result (make-run-config "maci-9b1b1a6-fixed/quinTreeCheckRoot_test.circom") 'safe)

  (check-result (make-run-config "maci-9b1b1a6-fixed/calculateTotal_test.circom") 'safe)
  (check-result (make-run-config "maci-9b1b1a6-fixed/decrypt_test.circom") 'safe)
  (check-result (make-run-config "maci-9b1b1a6-fixed/ecdh_test.circom") 'timeout)
  (check-result (make-run-config "maci-9b1b1a6-fixed/hasher11_test.circom") 'safe)
  (check-result (make-run-config "maci-9b1b1a6-fixed/hasher5_test.circom") 'safe)
  (check-result (make-run-config "maci-9b1b1a6-fixed/hashleftright_test.circom") 'safe)
  (check-result (make-run-config "maci-9b1b1a6-fixed/merkleTreeCheckRoot_test.circom") 'safe)
  (check-result (make-run-config "maci-9b1b1a6-fixed/merkleTreeInclusionProof_test.circom") 'safe)
  (check-result (make-run-config "maci-9b1b1a6-fixed/merkleTreeLeafExists_test.circom") 'safe)
  (check-result (make-run-config "maci-9b1b1a6-fixed/performChecksBeforeUpdate_test.circom") 'timeout)
  (check-result (make-run-config "maci-9b1b1a6-fixed/publicKey_test.circom") 'timeout)
  (check-result (make-run-config "maci-9b1b1a6-fixed/quinTreeInclusionProof_test.circom") 'timeout)
  (check-result (make-run-config "maci-9b1b1a6-fixed/quinTreeLeafExists_test.circom") 'safe)
  (check-result (make-run-config "maci-9b1b1a6-fixed/resultCommitmentVerifier_test.circom") 'safe)
  (check-result (make-run-config "maci-9b1b1a6-fixed/splicer_test.circom") 'timeout)
  (check-result (make-run-config "maci-9b1b1a6-fixed/updateStateTree_test.circom") 'timeout)
  (check-result (make-run-config "maci-9b1b1a6-fixed/verifySignature_test.circom") 'timeout))
