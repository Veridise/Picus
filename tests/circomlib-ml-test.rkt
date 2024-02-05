#lang racket/base

(module+ test
  (require "testlib.rkt")

  (current-solver solver:cvc5-bitsum)

  (check-result (make-run-config "circomlib-ml-adb9edd/Conv1D_test.circom") 'safe)
  (check-result (make-run-config "circomlib-ml-adb9edd/Conv2D_test.circom") 'safe)
  (check-result (make-run-config "circomlib-ml-adb9edd/Dense_test.circom") 'safe)
  (check-result (make-run-config "circomlib-ml-adb9edd/IsNegative_test.circom") 'timeout)
  (check-result (make-run-config "circomlib-ml-adb9edd/IsPositive_test.circom") 'timeout)
  (check-result (make-run-config "circomlib-ml-adb9edd/ReLU_test.circom") 'timeout)
  (check-result (make-run-config "circomlib-ml-adb9edd/SumPooling2D_test.circom") 'safe)
  (check-result (make-run-config "circomlib-ml-adb9edd/mnist_convnet_test.circom" #:timeout 180) 'safe)
  (check-result (make-run-config "circomlib-ml-adb9edd/mnist_poly_test.circom") 'safe)
  (check-result (make-run-config "circomlib-ml-adb9edd/mnist_test.circom") 'timeout)
  (check-result (make-run-config "circomlib-ml-adb9edd/model1_test.circom") 'timeout))
