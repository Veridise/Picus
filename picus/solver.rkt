#lang racket

(provide z3
         cvc4
         cvc5)

(require "solver-helper.rkt"

         (prefix-in config: "config.rkt")
         (prefix-in r1cs: "r1cs/r1cs-grammar.rkt")

         (prefix-in z3-rint: "./r1cs/r1cs-z3-interpreter.rkt")
         (prefix-in z3-parser: "./r1cs/r1cs-z3-parser.rkt")
         ; optimizers
         (prefix-in z3-simple: "./optimizers/r1cs-z3-simple-optimizer.rkt")
         (prefix-in z3-subp: "./optimizers/r1cs-z3-subp-optimizer.rkt")
         (prefix-in z3-ab0: "./optimizers/r1cs-z3-ab0-optimizer.rkt")

         (prefix-in cvc4-rint: "./r1cs/r1cs-cvc4-interpreter.rkt")

         (prefix-in cvc5-rint: "./r1cs/r1cs-cvc5-interpreter.rkt")
         (prefix-in cvc5-parser: "./r1cs/r1cs-cvc5-parser.rkt")
         ; optimizers
         (prefix-in cvc5-simple: "./optimizers/r1cs-cvc5-simple-optimizer.rkt")
         (prefix-in cvc5-subp: "./optimizers/r1cs-cvc5-subp-optimizer.rkt")
         (prefix-in cvc5-ab0: "./optimizers/r1cs-cvc5-ab0-optimizer.rkt"))

(define solver-interface<%>
  (interface ()
    solve
    get-options
    parse-r1cs
    expand-r1cs
    normalize-r1cs
    ; phase 0 optimization, applies to standard form
    optimize-r1cs-p0
    ; phase 1 optimization, applies to normalized form
    ;   - pdecl?: whether or not to inlude declaration of p, usually alt- series should not include
    optimize-r1cs-p1
    encode-smt
    get-name))

(define z3%
  (class* object% (solver-interface<%>)
    (super-new)
    (define/public (solve smt-str timeout #:verbose? [verbose? #f])
      ((make-solve #:executable "z3") smt-str timeout #:verbose? verbose?))

    (define/public (get-options)
      (list (r1cs:rlogic "QF_NIA")))

    (define/public (parse-r1cs arg-r1cs prefix)
      (z3-parser:parse-r1cs arg-r1cs prefix))

    (define/public (expand-r1cs arg-r1cs)
      (z3-parser:expand-r1cs arg-r1cs))

    (define/public (normalize-r1cs arg-r1cs)
      (z3-simple:optimize-r1cs arg-r1cs))

    (define/public (optimize-r1cs-p0 arg-r1cs)
      (z3-ab0:optimize-r1cs arg-r1cs))

    (define/public (optimize-r1cs-p1 arg-r1cs pdef?)
      (z3-subp:optimize-r1cs arg-r1cs pdef?))

    (define/public (encode-smt arg-r1cs)
      (z3-rint:interpret-r1cs arg-r1cs))

    (define/public (get-name) "z3")))

;; Most methods in cvc4 is shared with z3 (and not cvc5, since cvc4
;; doesn't support the finite field theory in cvc5). So we inherit cvc4 from z3.
(define cvc4%
  (class* z3% (solver-interface<%>)
    (super-new)
    (define/override (solve smt-str timeout #:verbose? [verbose? #f])
      ((make-solve #:executable "cvc4" #:options '("--produce-models")) smt-str timeout #:verbose? verbose?))

    (define/override (encode-smt arg-r1cs)
      (cvc4-rint:interpret-r1cs arg-r1cs))

    (define/override (get-name) "cvc4")))

(define cvc5%
  (class* object% (solver-interface<%>)
    (super-new)
    (define/public (solve smt-str timeout #:verbose? [verbose? #f])
      ((make-solve #:executable "cvc5" #:options '("--produce-models")) smt-str timeout #:verbose? verbose?))

    (define/public (get-options)
      (list
       (r1cs:rlogic "QF_FF")
       (r1cs:rraw "(set-info :smt-lib-version 2.6)")
       (r1cs:rraw "(set-info :category \"crafted\")")
       (r1cs:rraw (format "(define-sort F () (_ FiniteField ~a))" config:p))))

    (define/public (parse-r1cs arg-r1cs prefix)
      (cvc5-parser:parse-r1cs arg-r1cs prefix))

    (define/public (expand-r1cs arg-r1cs)
      (cvc5-parser:expand-r1cs arg-r1cs))

    (define/public (normalize-r1cs arg-r1cs)
      (cvc5-simple:optimize-r1cs arg-r1cs))

    (define/public (optimize-r1cs-p0 arg-r1cs)
      (cvc5-ab0:optimize-r1cs arg-r1cs))

    (define/public (optimize-r1cs-p1 arg-r1cs pdef?)
      (cvc5-subp:optimize-r1cs arg-r1cs pdef?))

    (define/public (encode-smt arg-r1cs)
      (cvc5-rint:interpret-r1cs arg-r1cs))

    (define/public (get-name) "cvc5")))

(define z3 (new z3%))
(define cvc4 (new cvc4%))
(define cvc5 (new cvc5%))
