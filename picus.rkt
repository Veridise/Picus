#lang racket
; common require
(require (prefix-in tokamak: "./picus/tokamak.rkt")
         (prefix-in utils: "./picus/utils.rkt")
         (prefix-in config: "./picus/config.rkt")
         (prefix-in solver: "./picus/solver.rkt")
         (prefix-in r1cs: "./picus/r1cs/r1cs-grammar.rkt")
         (prefix-in dpvl: "./picus/algorithms/dpvl.rkt")
         (prefix-in pre: "./picus/precondition.rkt")
         "ansi.rkt"
         "verbose.rkt")

; =====================================
; ======== commandline parsing ========
; =====================================
; parse command line arguments
(define arg-r1cs null)
(define arg-timeout 5000)
(define arg-solver "z3")
(define arg-selector "counter")
(define arg-precondition null)
(define arg-prop #t)
(define arg-slv #t)
(define arg-smt #f)
(define arg-weak #f)
(define arg-cex-verbose 0)
(command-line
 #:once-each
 [("--r1cs") p-r1cs "path to target r1cs"
             (set! arg-r1cs p-r1cs)
             (when (not (string-suffix? arg-r1cs ".r1cs"))
               (tokamak:exit "file needs to be *.r1cs"))]
 [("--timeout") p-timeout "timeout for every small query (default: 5000ms)"
                (set! arg-timeout (string->number p-timeout))]
 [("--solver") p-solver "solver to use: z3 | cvc4 | cvc5 (default: z3)"
               (cond
                 [(set-member? (set "z3" "cvc5" "cvc4") p-solver) (set! arg-solver p-solver)]
                 [else (tokamak:exit "solver needs to be either z3 or cvc5")])]
 [("--selector") p-selector "selector to use: first | counter (default: counter)"
                 (set! arg-selector p-selector)]
 [("--precondition") p-precondition "path to precondition json (default: null)"
                     (set! arg-precondition p-precondition)]
 [("--noprop") "disable propagation (default: false / propagation on)"
               (set! arg-prop #f)]
 [("--nosolve") "disable solver phase (default: false / solver on)"
                (set! arg-slv #f)]
 [("--smt") "show path to generated smt files (default: false)"
            (set! arg-smt #t)]
 [("--weak") "only check weak safety, not strong safety  (default: false)"
             (set! arg-weak #t)]
 [("--verbose")
  verbose
  ["verbose level (default: 0)"
   "  0: not verbose; output algorithm computation minimally"
   "  1: output algorithm computation, but display ... when the output is too large"
   "  2: output full algorithm computation"]
  (set-verbose! (match verbose
                  [(or "0" "1" "2") (string->number verbose)]
                  [_ (tokamak:exit "unrecognized verbose level: ~a" verbose)]))]
 [("--cex-verbose") cex-verbose
                    ["counterexample verbose level (default: 0)"
                     "  0: not verbose; only output with circom variable format"
                     "  1: output with circom variable format when applicable, and r1cs signal format otherwise"
                     "  2: output with r1cs signal format"]
                    (set! arg-cex-verbose
                          (match cex-verbose
                            [(or "0" "1" "2") (string->number cex-verbose)]
                            [_ (tokamak:exit "unrecognized verbose level: ~a" cex-verbose)]))])
(printf "# r1cs file: ~a\n" arg-r1cs)
(printf "# timeout: ~a\n" arg-timeout)
(printf "# solver: ~a\n" arg-solver)
(printf "# selector: ~a\n" arg-selector)
(printf "# precondition: ~a\n" arg-precondition)
(printf "# propagation on: ~a\n" arg-prop)
(printf "# solver on: ~a\n" arg-slv)
(printf "# smt: ~a\n" arg-smt)
(printf "# weak: ~a\n" arg-weak)
(printf "# cex-verbose: ~a\n" arg-cex-verbose)

; =================================================
; ======== resolve solver specific methods ========
; =================================================
(define solve (solver:solve arg-solver))
(define parse-r1cs (solver:parse-r1cs arg-solver))
(define expand-r1cs (solver:expand-r1cs arg-solver))
(define normalize-r1cs (solver:normalize-r1cs arg-solver))
(define optimize-r1cs-p0 (solver:optimize-r1cs-p0 arg-solver))
(define optimize-r1cs-p1 (solver:optimize-r1cs-p1 arg-solver))
(define interpret-r1cs (solver:interpret-r1cs arg-solver))

; ==================================
; ======== main preparation ========
; ==================================
(define r0 (r1cs:read-r1cs arg-r1cs))
(define nwires (r1cs:get-nwires r0))
(define mconstraints (r1cs:get-mconstraints r0))
(printf "# number of wires: ~a\n" nwires)
(printf "# number of constraints: ~a\n" mconstraints)
(printf "# field size (how many bytes): ~a\n" (r1cs:get-field-size r0))
(printf "# prime number: ~a\n" (r1cs:get-prime-number r0))
(config:set-p! (r1cs:get-prime-number r0))

; categorize signals
(define input-list (r1cs:r1cs-inputs r0))
(define input-set (list->set input-list))
(define output-list (r1cs:r1cs-outputs r0))
(define output-set (list->set output-list))
(define target-set (if arg-weak (list->set output-list) (list->set (range nwires))))
(printf "# inputs: ~a.\n" input-list)
(printf "# outputs: ~a.\n" output-list)
(printf "# targets: ~a.\n" target-set)

; parse original r1cs
(printf "# parsing original r1cs...\n")
;; invariant: (length varlist) = nwires
(define-values (varlist opts defs cnsts) (parse-r1cs r0 '())) ; interpret the constraint system
(vprintf "# varlist: ~e.\n" varlist)
; parse alternative r1cs
(define alt-varlist
  (for/list ([i (in-range nwires)] [var (in-list varlist)])
    (if (not (utils:contains? input-list i))
        (format "y~a" i)
        var)))
(vprintf "# alt-varlist ~e.\n" alt-varlist)
(printf "# parsing alternative r1cs...\n")
(define-values (_ __ alt-defs alt-cnsts) (parse-r1cs r0 alt-varlist))

(printf "# configuring precondition...\n")
(define-values (unique-set precondition)
  (if (null? arg-precondition)
      (values (set) '())
      (pre:read-precondition arg-precondition))) ; read!
(printf "# unique: ~a.\n" unique-set)

; ============================
; ======== main solve ========
; ============================
; a full picus constraint pass is:
;   raw
;    | parse-r1cs
;    v
;  cnsts
;    | optimize-r1cs-p0
;    v
; p0cnsts
;    | expand-r1cs
;    v
; expcnsts
;    | normalize-r1cs
;    v
; nrmcnsts
;    | optimize-r1cs-p1
;    v
; p1cnsts
;    | (downstream queries)
;   ...
(define path-sym (string-replace arg-r1cs ".r1cs" ".sym"))
(define-values (res res-ks res-us res-info)
  (dpvl:apply-algorithm
   r0 nwires mconstraints
   input-set output-set target-set
   varlist opts defs cnsts
   alt-varlist alt-defs alt-cnsts
   unique-set precondition ; prior knowledge row
   arg-selector arg-prop arg-slv arg-timeout arg-smt arg-cex-verbose path-sym
   solve interpret-r1cs
   optimize-r1cs-p0 expand-r1cs normalize-r1cs optimize-r1cs-p1))
(printf "# final unknown set ~a.\n" res-us)
(if arg-weak
    (printf "# weak uniqueness: ~a.\n" res)
    (printf "# strong uniqueness: ~a.\n" res))

;; format-cex :: string?, (listof (pairof string? any/c)), #:diff (listof (pairof string? any/c)) -> void?
(define (format-cex heading info #:diff [diff info])
  (printf "  # ~a:\n" heading)
  (for ([entry (in-list info)] [diff-entry (in-list diff)])
    (printf (cond
              [(equal? (cdr entry) (cdr diff-entry))
               "    # ~a: ~a\n"]
              [else (highlight "    # ~a: ~a\n")])
            (car entry) (cdr entry)))
  (when (empty? info)
    (printf "    # no ~a\n" heading)))

;; order :: hash? -> (listof (pairof string? any/c))
(define (order info)
  (sort (hash->list info) string<? #:key car))

(when (equal? 'unsafe res)
  (printf "# ~a is underconstrained. Below is a counterexample:\n" arg-r1cs)
  (match-define (list input-info output1-info output2-info other-info) res-info)
  (define output1-ordered (order output1-info))
  (define output2-ordered (order output2-info))

  (format-cex "inputs" (order input-info))
  (format-cex "first possible outputs" output1-ordered #:diff output2-ordered)
  (format-cex "second possible outputs" output2-ordered #:diff output1-ordered)
  (when (> arg-cex-verbose 0)
    (format-cex "other bindings" (order other-info))))
