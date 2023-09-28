#lang racket
; common require
(require (prefix-in tokamak: "./picus/tokamak.rkt")
         (prefix-in utils: "./picus/utils.rkt")
         (prefix-in config: "./picus/config.rkt")
         (prefix-in solver: "./picus/solver.rkt")
         (prefix-in r1cs: "./picus/r1cs/r1cs-grammar.rkt")
         (prefix-in dpvl: "./picus/algorithms/dpvl.rkt")
         (prefix-in pre: "./picus/precondition.rkt")
         "picus/tmpdir.rkt"
         "picus/ansi.rkt"
         "picus/verbose.rkt"
         "picus/global-inputs.rkt")

; =====================================
; ======== commandline parsing ========
; =====================================
; parse command line arguments
(define arg-r1cs #f)
(define arg-circom #f)
(define arg-patch? #f)
(define arg-opt-level #f)
(define arg-clean? #t)
(define arg-timeout 5000)
(define arg-solver "cvc5")
(define arg-selector "counter")
(define arg-precondition null)
(define arg-prop #t)
(define arg-slv #t)
(define arg-smt #f)
(define arg-strong #f)
(define arg-cex-verbose 0)
(command-line
 #:once-any
 [("--r1cs") p-r1cs "path to target r1cs"
             (set! arg-r1cs p-r1cs)
             (when (not (string-suffix? arg-r1cs ".r1cs"))
               (tokamak:exit "file needs to be *.r1cs"))]
 [("--circom") p-circom "path to target circom (need circom compiler in PATH)"
               (set! arg-circom p-circom)
               (when (not (string-suffix? arg-circom ".circom"))
                 (tokamak:exit "file needs to be *.circom"))]
 #:once-each
 [("--noclean") "do not clean up temporary files (default: false)"
                (set! arg-clean? #f)]
 [("--patch-circom") "patch circom file to add public inputs (only applicable for --circom, default: false)"
                     (set! arg-patch? #t)]
 [("--opt-level") p-opt-level "optimization level for circom compilation (only applicable for --circom, default: 0)"
                  (set! arg-opt-level
                        (match p-opt-level
                          [(or "0" "1" "2") p-opt-level]
                          [_ (tokamak:exit "unrecognized optimization level: ~a" p-opt-level)]))]
 [("--timeout") p-timeout "timeout for every small query (default: 5000ms)"
                (set! arg-timeout (string->number p-timeout))]
 [("--solver") p-solver "solver to use: z3 | cvc4 | cvc5 (default: cvc5)"
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
 [("--strong") "check for strong safety (default: false)"
               (set! arg-strong #t)]
 [("--verbose")
  verbose
  ["verbose level (default: 0)"
   "  0: not verbose; only display the final output"
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

(unless (or arg-r1cs arg-circom)
  (tokamak:exit "specify either --r1cs or --circom"))

(unless (implies arg-opt-level arg-circom)
  (tokamak:exit "--opt-level only applicable for --circom"))

(unless (implies arg-patch? arg-circom)
  (tokamak:exit "--patch-circom only applicable for --circom"))

;; compile-circom :: path? -> path?
;; compile circom file to r1cs file
(define (compile-circom circom-path)
  (unless (system* (find-executable-path "circom")
                   "-o"
                   (get-tmpdir)
                   "--r1cs"
                   circom-path
                   "--sym"
                   (match arg-opt-level
                     [(or #f "0") "--O0"]
                     ["1" "--O1"]
                     ["2" "--O2"]))
    (tokamak:exit "circom compilation failed"))
  (~a (build-path
       (get-tmpdir)
       (file-name-from-path (path-replace-extension circom-path ".r1cs")))))

;; compile+patch-circom :: path? boolean? -> (values path? r1cs?)
(define (compile+patch-circom circom-path patch?)
  (define r1cs-path (compile-circom circom-path))
  (cond
    [patch?
     (define r0 (r1cs:read-r1cs r1cs-path))
     (cond
       ;; no public inputs
       [(zero? (r1cs:get-npubin r0))
        (define patched-circom-path
          (path-replace-extension circom-path ".patched.circom"))

        (with-output-to-file patched-circom-path
          #:exists 'replace
          (λ ()
            ;; HACK: this assumes that the circom file has a line with "component main ="
            ;; which is the case for most circom files with no public inputs
            (displayln
             (string-replace
              (file->string circom-path)
              "component main ="
              (format "component main {public [~a]} ="
                      (string-join
                       (get-global-inputs
                        r1cs-path
                        (path-replace-extension r1cs-path ".sym"))
                       ", "))))))

        (define patched-r1cs-path (compile-circom patched-circom-path))
        (values patched-r1cs-path (r1cs:read-r1cs patched-r1cs-path))]
       ;; already has public inputs
       [else (values r1cs-path r0)])]
    ;; not patching
    [else (values r1cs-path (r1cs:read-r1cs r1cs-path))]))

(define-values (r1cs-path r0)
  (cond
    [arg-r1cs (values arg-r1cs (r1cs:read-r1cs arg-r1cs))]
    [else (compile+patch-circom arg-circom arg-patch?)]))

(vprintf "r1cs file: ~a\n" r1cs-path)
(vprintf "timeout: ~a\n" arg-timeout)
(vprintf "solver: ~a\n" arg-solver)
(vprintf "selector: ~a\n" arg-selector)
(vprintf "precondition: ~a\n" arg-precondition)
(vprintf "propagation on: ~a\n" arg-prop)
(vprintf "solver on: ~a\n" arg-slv)
(vprintf "smt: ~a\n" arg-smt)
(vprintf "strong: ~a\n" arg-strong)
(vprintf "cex-verbose: ~a\n" arg-cex-verbose)

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
(define nwires (r1cs:get-nwires r0))
(define mconstraints (r1cs:get-mconstraints r0))
(vprintf "number of wires: ~a\n" nwires)
(vprintf "number of constraints: ~a\n" mconstraints)
(vprintf "field size (how many bytes): ~a\n" (r1cs:get-field-size r0))
(vprintf "prime number: ~a\n" (r1cs:get-prime-number r0))
(config:set-p! (r1cs:get-prime-number r0))

; categorize signals
(define input-list (r1cs:r1cs-inputs r0))
(define input-set (list->set input-list))
(define output-list (r1cs:r1cs-outputs r0))
(define output-set (list->set output-list))
(define target-set (if arg-strong (list->set (range nwires)) (list->set output-list)))
(vprintf "inputs: ~e.\n" input-list)
(vprintf "outputs: ~e.\n" output-list)
(vprintf "targets: ~e.\n" target-set)

; parse original r1cs
(vprintf "parsing original r1cs...\n")
;; invariant: (length varlist) = nwires
(define-values (varlist opts defs cnsts) (parse-r1cs r0 '())) ; interpret the constraint system
(vprintf "varlist: ~e.\n" varlist)
; parse alternative r1cs
(define alt-varlist
  (for/list ([i (in-range nwires)] [var (in-list varlist)])
    (if (not (utils:contains? input-list i))
        (format "y~a" i)
        var)))
(vprintf "alt-varlist ~e.\n" alt-varlist)
(vprintf "parsing alternative r1cs...\n")
(define-values (_ __ alt-defs alt-cnsts) (parse-r1cs r0 alt-varlist))

(vprintf "configuring precondition...\n")
(define-values (unique-set precondition)
  (if (null? arg-precondition)
      (values (set) '())
      (pre:read-precondition arg-precondition))) ; read!
(vprintf "unique: ~a.\n" unique-set)

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
(define path-sym (string-replace r1cs-path ".r1cs" ".sym"))
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
(vprintf "final unknown set ~e.\n" res-us)
(printf "~a uniqueness: ~a.\n" (if arg-strong "strong" "weak") res)

;; format-cex :: string?, (listof (pairof string? any/c)), #:diff (listof (pairof string? any/c)) -> void?
(define (format-cex heading info #:diff [diff info])
  (printf "  ~a:\n" heading)
  (for ([entry (in-list info)] [diff-entry (in-list diff)])
    (printf (cond
              [(equal? (cdr entry) (cdr diff-entry))
               "    ~a: ~a\n"]
              [else (highlight "    ~a: ~a\n")])
            (car entry) (cdr entry)))
  (when (empty? info)
    (printf "    no ~a\n" heading)))

;; order :: hash? -> (listof (pairof string? any/c))
(define (order info)
  (sort (hash->list info) string<? #:key car))

(when (equal? 'unsafe res)
  (printf "~a is underconstrained. Below is a counterexample:\n" r1cs-path)
  (match-define (list input-info output1-info output2-info other-info) res-info)
  (define output1-ordered (order output1-info))
  (define output2-ordered (order output2-info))

  (format-cex "inputs" (order input-info))
  (format-cex "first possible outputs" output1-ordered #:diff output2-ordered)
  (format-cex "second possible outputs" output2-ordered #:diff output1-ordered)
  (when (> arg-cex-verbose 0)
    (format-cex "other bindings" (order other-info))))

(when arg-clean?
  (clean-tmpdir!))
