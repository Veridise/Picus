#lang racket
(provide p
         set-p!)

; the global field p as seen in: https://docs.circom.io/circom-language/basic-operators/#field-elements
; also used by ecne as seen in: reference: https://github.com/franklynwang/EcneProject/blob/master/src/R1CSConstraintSolver.jl#L10
(define p 21888242871839275222246405745257275088548364400416034343698204186575808495617)

(define (set-p! val)
  (set! p val))
