#lang racket/base

(module+ test
  (require "testlib.rkt")

  (current-solver solver:cvc5-bitsum)

  (check-result (make-run-config "aes-circom-0784f74/aes_256_ctr_test.circom") 'timeout)
  (check-result (make-run-config "aes-circom-0784f74/aes_256_encrypt_test.circom") 'timeout)
  (check-result (make-run-config "aes-circom-0784f74/aes_256_key_expansion_test.circom") 'timeout)

  ;; The next two programs are invalid
  ;; [circom] error[T3001]: Component polyval_1 is created but not all its inputs are initialized
  #;(check-result (make-run-config "aes-circom-0784f74/gcm_siv_dec_2_keys_test") ...)
  #;(check-result (make-run-config "aes-circom-0784f74/gcm_siv_enc_2_keys_test") ...)

  (check-result (make-run-config "aes-circom-0784f74/gfmul_int_test.circom") 'safe)
  (check-result (make-run-config "aes-circom-0784f74/mul_test.circom") 'safe)
  (check-result (make-run-config "aes-circom-0784f74/polyval_test.circom") 'timeout))
