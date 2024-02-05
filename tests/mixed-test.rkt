#lang racket/base

(module+ test
  (require "testlib.rkt")

  (current-solver solver:cvc5-bitsum)

  (check-result (make-run-config "hydra-2010a65/hydra-s1.circom") 'safe)
  (check-result (make-run-config "secureum-zk-2023/unary.circom") 'safe)
  (check-result (make-run-config "semaphore-0f0fc95/semaphore.circom") 'safe)

  (check-result (make-run-config "zk-group-sigs-1337689-fixed/deny.circom") 'safe)
  (check-result (make-run-config "zk-group-sigs-1337689-fixed/reveal.circom") 'safe)
  ;; This fails to compile with:
  ;; error[T3001]: Component is_hash_present is created but not all its inputs are initialized
  #;(check-result (make-run-config "zk-group-sigs-1337689-fixed/sign.circom") '...)

  (check-result (make-run-config "zk-SQL-4c3626d/delete.circom") 'timeout)
  (check-result (make-run-config "zk-SQL-4c3626d/insert.circom") 'safe)
  (check-result (make-run-config "zk-SQL-4c3626d/select.circom") 'safe)
  (check-result (make-run-config "zk-SQL-4c3626d/update.circom") 'timeout)

  (check-result (make-run-config "civer-comparison/ConstrainedDecoder.circom") 'safe))
