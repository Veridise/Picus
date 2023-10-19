#lang racket
; this implements the linear lemma:
;   if c * x = (unique), and c != 0 is a constant, then x is also uniquely determined
; note that this lemma doesn't apply to the following:
;   c * x0 * x1 = (unique), and c * x0 != 0
(require (prefix-in r1cs: "../../r1cs/r1cs-grammar.rkt")
         "../../logging.rkt")
(provide compute-linear-clauses
         compute-weight-map
         apply-lemma)

; get constraint dependency clauses
; input is the *normalized main constraint part* of r1cs ast
;   - main constraints is the `cnsts part (r1cs:rcmds) from parse-r1cs
;
; returns a (listof (list/c integer? mutable-set? mutable-set?)):
;   where each tuple item in the list corresponds to a constraint.
;   The tuple consists of:
;   - i: index
;   - deducible-vars: a set of variables that can be determined as unique in the current constraint
;   - nonlinear-vars: a set of variables that are non-linear in the current constraint
;
; meaning:
;   in a clause, to determine that a var in deducible-vars is unique,
;   all nonlinear-vars and other deducible-vars must be determined unique.
;
; construction rules (++terms):
;   - only non-non-linear (YES, no typo here) variable can be determined (put to key)
;     because for x*y=k, x can't be guaranteed to be unique,
;     even if knowing y and k (due to field mul)
(define (compute-linear-clauses arg-cnsts [arg-indexonly #f])
  (for/list ([p (r1cs:rcmds-vs arg-cnsts)]
             [i (in-naturals)]
             #:do [(define all-vars (r1cs:get-assert-variables p arg-indexonly))
                   (define nonlinear-vars (r1cs:get-assert-variables/nonlinear p arg-indexonly))
                   ; (note) you can't use linears directly, because one var could be both linear and non-linear
                   ;        in this case, it's still non-linear in the current constraint
                   (define deducible-vars (set-subtract all-vars nonlinear-vars))
                   (picus:log-debug "[linear lemma] ~a: ~a | ~a" i deducible-vars nonlinear-vars)]
             #:unless (set-empty? deducible-vars))
    (list i (set-copy deducible-vars) (set-copy nonlinear-vars))))

(define (compute-weight-map linear-clauses)
  (define res (make-hash))
  (for ([clause (in-list linear-clauses)])
    (match-define (list _ deducible-vars nonlinear-vars) clause)
    ;; NOTE(sorawee): I think this is not optimal. We should revisit this.
    (define len-deducible (set-count deducible-vars))
    (for ([var (in-set deducible-vars)])
      (hash-update! res var (λ (old) (+ old len-deducible -1)) 0))
    (for ([var (in-set nonlinear-vars)])
      (hash-update! res var (λ (old) (+ old len-deducible)) 0)))
  res)

; recursively apply linear lemma
(define (apply-lemma linear-clauses ks us)
  (picus:log-progress "[linear lemma] starting propagation")
  ;; add a dummy element -1 so that working-set is not initially empty
  ;; (since being empty is the condition for termination)
  (let loop ([linear-clauses linear-clauses]
             [inferred (set)]
             [working-set (set-add ks -1)])
    (cond
      [(set-empty? working-set)
       (values linear-clauses (set-union ks inferred) (set-subtract us inferred))]
      [else
       ;; remove all known variables from linear-clauses
       (for ([pair (in-list linear-clauses)])
         (match-define (list i deducible-vars nonlinear-vars) pair)
         ;; make a copy via set->list so that we can remove items froim the set
         ;; while iterating on it.
         (for ([v (in-list (set->list deducible-vars))]
               #:when (set-member? working-set v))
           (picus:log-debug "[linear lemma] successful propagation of ~a on clause ~a" v i)
           (set-remove! deducible-vars v))

         (for ([v (in-list (set->list nonlinear-vars))]
               #:when (set-member? working-set v))
           (set-remove! nonlinear-vars v)))

       ;; filter out useless clauses
       (define new-linear-clauses
         (for/list ([pair (in-list linear-clauses)]
                   #:do [(match-define (list _ deducible-vars _) pair)]
                   #:unless (set-empty? deducible-vars))
           pair))

       ;; NOTE: possible optimization: remove these entries right away,
       ;; since the next iteration would remove them anyway.
       (define Δinferred
         (for/set ([pair (in-list new-linear-clauses)]
                   #:do [(match-define (list i deducible-vars nonlinear-vars) pair)]
                   #:when (= 1 (set-count deducible-vars))
                   #:when (set-empty? nonlinear-vars))
           (define v (set-first deducible-vars))
           (picus:log-debug "[linear lemma] deduced ~a from clause ~a" v i)
           v))

       (loop new-linear-clauses (set-union inferred Δinferred) Δinferred)])))
