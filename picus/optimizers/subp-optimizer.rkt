#lang racket
; (note) this is applied in optimization phase 1
; this contains the following optimizations:
;   - add p related definition and replace p
(require "../exit.rkt"
         (prefix-in config: "../config.rkt")
         (prefix-in r1cs: "../r1cs/r1cs-grammar.rkt"))
(provide optimize-subp)

(define (optimize-subp e proc)
  (let loop ([e e])
    (proc
     e
     (λ (e)
       (match e
         ; command level
         [(r1cs:rcmds vs) (r1cs:rcmds (for/list ([v vs]) (loop v)))]
         [(r1cs:rraw v) (r1cs:rraw v)]
         [(r1cs:rlogic v) (r1cs:rlogic v)]
         ; (note) don't optimize declaration line
         [(r1cs:rdef v t) (r1cs:rdef v (loop t))]
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

         [(r1cs:rand vs) (r1cs:rand (for/list ([v vs]) (loop v)))]
         [(r1cs:ror vs) (r1cs:ror (for/list ([v vs]) (loop v)))]

         [(r1cs:rint v)
          (cond
            [(= (- config:p 1) v) (r1cs:rvar "ps1")]
            [(= (- config:p 2) v) (r1cs:rvar "ps2")]
            [(= (- config:p 3) v) (r1cs:rvar "ps3")]
            [(= (- config:p 4) v) (r1cs:rvar "ps4")]
            [(= (- config:p 5) v) (r1cs:rvar "ps5")]
            ; replace as zero
            [(= 0 v) (r1cs:rvar "zero")]
            ; replace as one
            [(= 1 v) (r1cs:rvar "one")]
            ; do nothing
            [else (r1cs:rint v)])]
         [(r1cs:rvar v) (r1cs:rvar v)]
         [(r1cs:rtype v) (r1cs:rtype v)]

         [(r1cs:radd vs) (r1cs:radd (for/list ([v vs]) (loop v)))]
         [(r1cs:rmul vs) (r1cs:rmul (for/list ([v vs]) (loop v)))]
         [(r1cs:rmod v mod) (r1cs:rmod (loop v) (loop mod))]

         [_ (picus:tool-error "not supported: ~a" e)])))))
