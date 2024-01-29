#lang racket

(provide subst-vars
         subst-vars*
         convert-y)

(require (prefix-in r1cs: "r1cs/r1cs-grammar.rkt")
         "exit.rkt")

(define (subst-vars e proc)
  (let subst-vars ([e e])
    (match e

      ; command level
      [(r1cs:rcmds vs) (r1cs:rcmds (for/list ([v vs]) (subst-vars v)))]

      [(r1cs:rraw v) (r1cs:rraw v)]
      [(r1cs:rlogic v) (r1cs:rlogic v)]
      ; (note) don't optimize declaration line
      [(r1cs:rdef v t) (r1cs:rdef v (subst-vars t))]
      [(r1cs:rassert v) (r1cs:rassert (subst-vars v))]
      [(r1cs:rcmt v) (r1cs:rcmt v)]
      [(r1cs:rsolve) (r1cs:rsolve)]

      ; sub-command level
      [(r1cs:req lhs rhs) (r1cs:req (subst-vars lhs) (subst-vars rhs))]
      [(r1cs:rneq lhs rhs) (r1cs:rneq (subst-vars lhs) (subst-vars rhs))]
      [(r1cs:rlt lhs rhs) (r1cs:rlt (subst-vars lhs) (subst-vars rhs))]

      [(r1cs:rand vs) (r1cs:rand (for/list ([v vs]) (subst-vars v)))]
      [(r1cs:ror vs) (r1cs:ror (for/list ([v vs]) (subst-vars v)))]
      [(r1cs:rint v) (r1cs:rint v)]
      [(r1cs:rvar v)
       (match v
         [(pregexp #px"^x([0-9]+)$" (list _ (app string->number n))) (proc n)]
         [_ (picus:tool-error "unexpected variable: ~a" v)])]
      [(r1cs:rtype v) (r1cs:rtype v)]

      [(r1cs:radd vs) (r1cs:radd (for/list ([v vs]) (subst-vars v)))]
      [(r1cs:rmul vs) (r1cs:rmul (for/list ([v vs]) (subst-vars v)))]

      [_ (picus:tool-error "unknown expression: ~e" e)])))

(define (subst-vars* constraints proc)
  (for/list ([c (in-list constraints)])
    (subst-vars c proc)))

(define (convert-y n)
  (r1cs:rvar (format "y~a" n)))
