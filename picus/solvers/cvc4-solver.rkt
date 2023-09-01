#lang racket
(require "common.rkt")
(provide solve)

; solving component
(define solve (make-solve #:executable "cvc4" #:options '("--produce-models")))
