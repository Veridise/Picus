#lang racket/base

(provide concretize)
(require racket/match
         (prefix-in r1cs: "r1cs/r1cs-grammar.rkt")
         "exit.rkt")

;; concretize :: ast? string? -> ast?
(define (concretize e prefix)
  (let loop ([e e])
    (match e

      ; command level
      [(r1cs:rcmds vs) (r1cs:rcmds (for/list ([v (in-list vs)]) (loop v)))]

      [(r1cs:rraw v) (r1cs:rraw v)]
      [(r1cs:rlogic v) (r1cs:rlogic v)]
      [(r1cs:rdef v t) (r1cs:rdef (loop v) (loop t))]
      [(r1cs:rassert v) (r1cs:rassert (loop v))]
      [(r1cs:rcmt v) (r1cs:rcmt v)]
      [(r1cs:rsolve) (r1cs:rsolve)]

      ; sub-command level
      [(r1cs:req lhs rhs) (r1cs:req (loop lhs) (loop rhs))]
      [(r1cs:rneq lhs rhs) (r1cs:rneq (loop lhs) (loop rhs))]
      [(r1cs:rleq lhs rhs) (r1cs:rleq (loop lhs) (loop rhs))]
      [(r1cs:rlt lhs rhs) (r1cs:rlt (loop lhs) (loop rhs))]
      [(r1cs:rgeq lhs rhs) (r1cs:rgeq (loop lhs) (loop rhs))]
      [(r1cs:rgt lhs rhs) (r1cs:rgt (loop lhs) (loop rhs))]

      [(r1cs:rand vs) (r1cs:rand (for/list ([v (in-list vs)]) (loop v)))]
      [(r1cs:ror vs) (r1cs:ror (for/list ([v (in-list vs)]) (loop v)))]
      [(r1cs:rimp lhs rhs) (r1cs:rimp (loop lhs) (loop rhs))]
      [(r1cs:rint v) (r1cs:rint v)]
      [(r1cs:rvar v)
       (cond
         [(string? v) e]
         [else (r1cs:rvar (format "~a~a" prefix v))])]
      [(r1cs:rtype v) (r1cs:rtype v)]

      [(r1cs:radd vs) (r1cs:radd (for/list ([v (in-list vs)]) (loop v)))]
      [(r1cs:rmul vs) (r1cs:rmul (for/list ([v (in-list vs)]) (loop v)))]

      [(r1cs:rmod v mod) (r1cs:rmod (loop v) (loop mod))]

      [_ (picus:tool-error "unknown expression: ~e" e)])))
