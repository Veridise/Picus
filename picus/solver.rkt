#lang racket
; switcher for solver related components
(require json racket/engine
    (prefix-in tokamak: "./tokamak.rkt")
)
; z3 require
(require
    (prefix-in z3-solver: "./solvers/z3-solver.rkt")
    (prefix-in z3-rint: "./r1cs/r1cs-z3-interpreter.rkt")
    (prefix-in z3-parser: "./r1cs/r1cs-z3-parser.rkt")
    ; optimizers
    (prefix-in z3-simple: "./optimizers/r1cs-z3-simple-optimizer.rkt")
    (prefix-in z3-subp: "./optimizers/r1cs-z3-subp-optimizer.rkt")
    (prefix-in z3-ab0: "./optimizers/r1cs-z3-ab0-optimizer.rkt")
)
; cvc4 require (partly shared with z3)
(require
    (prefix-in cvc4-solver: "./solvers/cvc4-solver.rkt")
    (prefix-in cvc4-rint: "./r1cs/r1cs-cvc4-interpreter.rkt")
)
; cvc5 require
(require
    (prefix-in cvc5-solver: "./solvers/cvc5-solver.rkt")
    (prefix-in cvc5-rint: "./r1cs/r1cs-cvc5-interpreter.rkt")
    (prefix-in cvc5-parser: "./r1cs/r1cs-cvc5-parser.rkt")
    ; optimizers
    (prefix-in cvc5-simple: "./optimizers/r1cs-cvc5-simple-optimizer.rkt")
    (prefix-in cvc5-subp: "./optimizers/r1cs-cvc5-subp-optimizer.rkt")
    (prefix-in cvc5-ab0: "./optimizers/r1cs-cvc5-ab0-optimizer.rkt")
)
(provide (rename-out
    [solve solve]
    [parse-r1cs parse-r1cs]
    [expand-r1cs expand-r1cs]
    [normalize-r1cs normalize-r1cs]
    [optimize-r1cs-p0 optimize-r1cs-p0]
    [optimize-r1cs-p1 optimize-r1cs-p1]
    [interpret-r1cs interpret-r1cs]
))

(define (solve arg-solver)
    (cond
        [(equal? "z3" arg-solver) z3-solver:solve]
        [(equal? "cvc4" arg-solver) cvc4-solver:solve]
        [(equal? "cvc5" arg-solver) cvc5-solver:solve]
        [else (tokamak:exit "you can't reach here")]
    )
)
(define (parse-r1cs arg-solver)
    (cond
        [(equal? "z3" arg-solver) z3-parser:parse-r1cs]
        [(equal? "cvc4" arg-solver) z3-parser:parse-r1cs] ; share with z3
        [(equal? "cvc5" arg-solver) cvc5-parser:parse-r1cs]
        [else (tokamak:exit "you can't reach here")]
    )
)
(define (expand-r1cs arg-solver)
    (cond
        [(equal? "z3" arg-solver) z3-parser:expand-r1cs]
        [(equal? "cvc4" arg-solver) z3-parser:expand-r1cs] ; shared with z3
        [(equal? "cvc5" arg-solver) cvc5-parser:expand-r1cs]
    )
)
(define (normalize-r1cs arg-solver)
    (cond
        [(equal? "z3" arg-solver) (lambda (x) (z3-simple:optimize-r1cs x))]
        [(equal? "cvc4" arg-solver) (lambda (x) (z3-simple:optimize-r1cs x))] ; shared with z3
        [(equal? "cvc5" arg-solver) (lambda (x) (cvc5-simple:optimize-r1cs x))]
        [else (tokamak:exit "you can't reach here")]
    )
)
; phase 0 optimization, applies to standard form
(define (optimize-r1cs-p0 arg-solver)
    (cond
        [(equal? "z3" arg-solver) (lambda (x) (z3-ab0:optimize-r1cs x))]
        [(equal? "cvc4" arg-solver) (lambda (x) (z3-ab0:optimize-r1cs x))] ; shared with z3
        [(equal? "cvc5" arg-solver) (lambda (x) (cvc5-ab0:optimize-r1cs x))]
        [else (tokamak:exit "you can't reach here")]
    )
)
; phase 1 optimization, applies to normalized form
;   - pdecl?: whether or not to inlude declaration of p, usually alt- series should not include
(define (optimize-r1cs-p1 arg-solver)
    (cond
        [(equal? "z3" arg-solver) (lambda (x pdef?) (z3-subp:optimize-r1cs x pdef?))]
        [(equal? "cvc4" arg-solver) (lambda (x pdef?) (z3-subp:optimize-r1cs x pdef?))] ; shared with z3
        [(equal? "cvc5" arg-solver) (lambda (x pdef?) (cvc5-subp:optimize-r1cs x pdef?))]
        [else (tokamak:exit "you can't reach here")]
    )
)
(define (interpret-r1cs arg-solver)
    (cond
        [(equal? "z3" arg-solver) z3-rint:interpret-r1cs]
        [(equal? "cvc4" arg-solver) cvc4-rint:interpret-r1cs]
        [(equal? "cvc5" arg-solver) cvc5-rint:interpret-r1cs]
        [else (tokamak:exit "you can't reach here")]
    )
)
