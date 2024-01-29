#lang racket

(provide z3
         cvc4
         cvc5)

(require "solver-helper.rkt"
         "encoder.rkt"
         "exit.rkt"
         "optimizers/subp-optimizer.rkt"

         (prefix-in config: "config.rkt")
         (prefix-in r1cs: "r1cs/r1cs-grammar.rkt")

         (prefix-in z3-parser: "./r1cs/r1cs-z3-parser.rkt")
         ; optimizers
         (prefix-in z3-simple: "./optimizers/r1cs-z3-simple-optimizer.rkt")
         (prefix-in z3-ab0: "./optimizers/r1cs-z3-ab0-optimizer.rkt")

         (prefix-in cvc5-parser: "./r1cs/r1cs-cvc5-parser.rkt")
         ; optimizers
         (prefix-in cvc5-simple: "./optimizers/r1cs-cvc5-simple-optimizer.rkt")
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
    get-pdefs
    ; phase 1 optimization, applies to normalized form
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

    (define/public (get-pdefs)
      (r1cs:rcmds
       (list
        (r1cs:rcmt "======== p definitions ========")
        (r1cs:rdef (r1cs:rvar "p") (r1cs:rtype "Int"))
        (r1cs:rassert (r1cs:req (r1cs:rvar "p") (r1cs:rint config:p)))
        (r1cs:rdef (r1cs:rvar "ps1") (r1cs:rtype "Int"))
        (r1cs:rassert (r1cs:req (r1cs:rvar "ps1") (r1cs:rint (- config:p 1))))
        (r1cs:rdef (r1cs:rvar "ps2") (r1cs:rtype "Int"))
        (r1cs:rassert (r1cs:req (r1cs:rvar "ps2") (r1cs:rint (- config:p 2))))
        (r1cs:rdef (r1cs:rvar "ps3") (r1cs:rtype "Int"))
        (r1cs:rassert (r1cs:req (r1cs:rvar "ps3") (r1cs:rint (- config:p 3))))
        (r1cs:rdef (r1cs:rvar "ps4") (r1cs:rtype "Int"))
        (r1cs:rassert (r1cs:req (r1cs:rvar "ps4") (r1cs:rint (- config:p 4))))
        (r1cs:rdef (r1cs:rvar "ps5") (r1cs:rtype "Int"))
        (r1cs:rassert (r1cs:req (r1cs:rvar "ps5") (r1cs:rint (- config:p 5))))
        ; add 0 definition
        (r1cs:rdef (r1cs:rvar "zero") (r1cs:rtype "Int"))
        (r1cs:rassert (r1cs:req (r1cs:rvar "zero") (r1cs:rint 0)))
        ; add 1 definition
        (r1cs:rdef (r1cs:rvar "one") (r1cs:rtype "Int"))
        (r1cs:rassert (r1cs:req (r1cs:rvar "one") (r1cs:rint 1))))))

    (define/public (optimize-r1cs-p1 e)
      (optimize-subp
       e
       (λ (e fallback)
         (match e
           [(r1cs:rint (== config:p)) (r1cs:rvar "p")]
           [_ (fallback e)]))))

    (define/public (encode-smt e)
      (encode
       e
       (λ (e fallback)
         (let loop ([e e])
           (match e
             [(r1cs:rmod v mod) (emit "(rem " (loop v) " " (loop mod) ")")]
             [_ (fallback e)])))))

    (define/public (get-name) "z3")))

;; Most methods in cvc4 is shared with z3 (and not cvc5, since cvc4
;; doesn't support the finite field theory in cvc5). So we inherit cvc4 from z3.
(define cvc4%
  (class* z3% (solver-interface<%>)
    (super-new)
    (define/override (solve smt-str timeout #:verbose? [verbose? #f])
      ((make-solve #:executable "cvc4" #:options '("--produce-models")) smt-str timeout #:verbose? verbose?))

    (define/override (encode-smt e)
      (encode
       e
       (λ (e fallback)
         (let loop ([e e])
           (match e
             ; use mod for cvc4 since there's no rem
             [(r1cs:rmod v mod) (emit "(mod " (loop v) " " (loop mod) ")")]
             [_ (fallback e)])))))

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

    (define/public (get-pdefs)
      (r1cs:rcmds
       (list
        ; add p definition
        (r1cs:rcmt "======== p definitions ========")
        (r1cs:rdef (r1cs:rvar "p") (r1cs:rtype "F"))
        (r1cs:rassert (r1cs:req (r1cs:rvar "p") (r1cs:rint config:p)))
        (r1cs:rdef (r1cs:rvar "ps1") (r1cs:rtype "F"))
        (r1cs:rassert (r1cs:req (r1cs:rvar "ps1") (r1cs:rint (- config:p 1))))
        (r1cs:rdef (r1cs:rvar "ps2") (r1cs:rtype "F"))
        (r1cs:rassert (r1cs:req (r1cs:rvar "ps2") (r1cs:rint (- config:p 2))))
        (r1cs:rdef (r1cs:rvar "ps3") (r1cs:rtype "F"))
        (r1cs:rassert (r1cs:req (r1cs:rvar "ps3") (r1cs:rint (- config:p 3))))
        (r1cs:rdef (r1cs:rvar "ps4") (r1cs:rtype "F"))
        (r1cs:rassert (r1cs:req (r1cs:rvar "ps4") (r1cs:rint (- config:p 4))))
        (r1cs:rdef (r1cs:rvar "ps5") (r1cs:rtype "F"))
        (r1cs:rassert (r1cs:req (r1cs:rvar "ps5") (r1cs:rint (- config:p 5))))
        ; add 0 definition
        (r1cs:rdef (r1cs:rvar "zero") (r1cs:rtype "F"))
        (r1cs:rassert (r1cs:req (r1cs:rvar "zero") (r1cs:rint 0)))
        ; add 1 definition
        (r1cs:rdef (r1cs:rvar "one") (r1cs:rtype "F"))
        (r1cs:rassert (r1cs:req (r1cs:rvar "one") (r1cs:rint 1))))))

    (define/public (optimize-r1cs-p1 e)
      (optimize-subp
       e
       (λ (e fallback)
         (match e
           [(r1cs:rint (== config:p)) (r1cs:rint 0)]
           [(or (r1cs:rleq _ _) (r1cs:rlt _ _) (r1cs:rgeq _ _) (r1cs:rgt _ _)
                (r1cs:rmod _ _))
            (picus:tool-error "not supported: ~a" e)]
           [_ (fallback e)]))))

    (define/public (encode-smt e)
      (encode
       e
       (λ (e fallback)
         (let loop ([e e])
           (match e
             [(r1cs:rlt a b) (format-op "ff.lt" loop (list a b))]
             [(r1cs:rint v) (printf "#f~am~a" v config:p)]
             [(r1cs:radd vs) (format-op "ff.add" loop vs)]
             [(r1cs:rmul vs) (format-op "ff.mul" loop vs)]
             [(or (r1cs:rleq _ _) (r1cs:rlt _ _) (r1cs:rgeq _ _) (r1cs:rgt _ _)
                  (r1cs:rmod _ _))
              (picus:tool-error "not supported: ~a" e)]
             [_ (fallback e)])))))

    (define/public (get-name) "cvc5")))

(define z3 (new z3%))
(define cvc4 (new cvc4%))
(define cvc5 (new cvc5%))
