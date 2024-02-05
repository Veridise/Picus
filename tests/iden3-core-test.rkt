#lang racket/base

(module+ test
  (require "testlib.rkt")

  (current-solver solver:cvc5-bitsum)

  (check-result (make-run-config "iden3-core-56a08f9/auth.circom") 'safe)
  (check-result (make-run-config "iden3-core-56a08f9/authWithRelayTest.circom") 'safe)
  (check-result (make-run-config "iden3-core-56a08f9/credentialAtomicQueryMTP.circom") 'safe)
  (check-result (make-run-config "iden3-core-56a08f9/credentialAtomicQueryMTPWithRelay.circom") 'safe)
  (check-result (make-run-config "iden3-core-56a08f9/credentialAtomicQuerySig.circom") 'safe)
  (check-result (make-run-config "iden3-core-56a08f9/idOwnershipBySignatureTest.circom") 'safe)
  (check-result (make-run-config "iden3-core-56a08f9/idOwnershipBySignatureWithRelayTest.circom") 'safe)
  (check-result (make-run-config "iden3-core-56a08f9/inTest.circom") 'timeout)
  (check-result (make-run-config "iden3-core-56a08f9/queryTest.circom") 'timeout)
  (check-result (make-run-config "iden3-core-56a08f9/stateTransition.circom") 'safe)
  (check-result (make-run-config "iden3-core-56a08f9/utils_GetValueByIndex.circom") 'safe)
  (check-result (make-run-config "iden3-core-56a08f9/utils_checkIdenStateMatchesRoots.circom") 'safe)
  (check-result (make-run-config "iden3-core-56a08f9/utils_getClaimExpiration.circom") 'timeout)
  (check-result (make-run-config "iden3-core-56a08f9/utils_getClaimSubjectOtherIden.circom") 'timeout)
  (check-result (make-run-config "iden3-core-56a08f9/utils_getSubjectLocation.circom") 'safe)
  (check-result (make-run-config "iden3-core-56a08f9/utils_isExpirable.circom") 'safe)
  (check-result (make-run-config "iden3-core-56a08f9/utils_isUpdatable.circom") 'safe)
  (check-result (make-run-config "iden3-core-56a08f9/utils_verifyClaimSignature.circom") 'safe)
  (check-result (make-run-config "iden3-core-56a08f9/utils_verifyCredentialSubject.circom") 'safe)
  (check-result (make-run-config "iden3-core-56a08f9/utils_verifyExpirationTime.circom") 'safe))
