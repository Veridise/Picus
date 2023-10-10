#lang racket
; common require
(require (prefix-in utils: "./picus/utils.rkt")
         (prefix-in config: "./picus/config.rkt")
         (prefix-in solver: "./picus/solver.rkt")
         (prefix-in r1cs: "./picus/r1cs/r1cs-grammar.rkt")
         (prefix-in dpvl: "./picus/algorithms/dpvl.rkt")
         (prefix-in pre: "./picus/precondition.rkt")
         "picus/tmpdir.rkt"
         "picus/ansi.rkt"
         "picus/global-inputs.rkt"
         "picus/logging.rkt"
         "picus/framework.rkt"
         "picus/exit.rkt"
         "picus/gen-witness.rkt")

; =====================================
; ======== commandline parsing ========
; =====================================
; parse command line arguments
(define arg-json? #f)
(define arg-truncate? 'unset)
(define arg-patch? #f)
(define arg-opt-level #f)
(define arg-clean? #t)
(define arg-timeout 5000)
(define arg-solver "cvc5")
(define arg-selector "counter")
(define arg-precondition null)
(define arg-prop #t)
(define arg-slv #t)
(define arg-strong #f)
(define arg-log-level #f)
(define arg-wtns #f)

(define (circom-file? path)
  (string-suffix? path ".circom"))

(define (r1cs-file? path)
  (string-suffix? path ".r1cs"))

(define source
(command-line
 #:usage-help "<source> must be a file with .circom or .r1cs extension"
 #:once-each
 [("--json") "enable json logging (default: false)"
             (set! arg-json? #t)]
 [("--noclean") "do not clean up temporary files (default: false)"
                (set! arg-clean? #f)]
 [("--patch-circom") "patch circom file to add public inputs (only applicable for circom source, default: false)"
                     (set! arg-patch? #t)]
 [("--opt-level") p-opt-level "optimization level for circom compilation (only applicable for circom source, default: 0)"
                  (set! arg-opt-level
                        (match p-opt-level
                          [(or "0" "1" "2") p-opt-level]
                          [_ (picus:user-error "unrecognized optimization level: ~a" p-opt-level)]))]
 [("--timeout") p-timeout "timeout for every small query (default: 5000ms)"
                (set! arg-timeout (string->number p-timeout))]
 [("--solver") p-solver "solver to use: z3 | cvc4 | cvc5 (default: cvc5)"
               (cond
                 [(set-member? (set "z3" "cvc5" "cvc4") p-solver) (set! arg-solver p-solver)]
                 [else (picus:user-error "solver needs to be either z3 or cvc5")])]
 [("--selector") p-selector "selector to use: first | counter (default: counter)"
                 (match p-selector
                   [(or "first" "counter") (set! arg-selector p-selector)]
                   [_ (picus:user-error "selector needs to be either first or counter")])]
 [("--precondition") p-precondition "path to precondition json (default: none)"
                     (set! arg-precondition p-precondition)]
 [("--noprop") "disable propagation (default: false / propagation on)"
               (set! arg-prop #f)]
 [("--nosolve") "disable solver phase (default: false / solver on)"
                (set! arg-slv #f)]
 [("--strong") "check for strong safety (default: false)"
               (set! arg-strong #t)]
 [("--wtns") p-wtns
             "wtns files output directory (default: don't output)"
             (set! arg-wtns p-wtns)]
 [("--truncate") p-truncate
                 "truncate overly long logged message: on | off (default: off for --json, on otherwise)"
                 (match p-truncate
                   ["on" (set! arg-truncate? #t)]
                   ["off" (set! arg-truncate? #f)]
                   [_ (picus:user-error "truncate mode can only be either on or off")])]
 [("--log-level") p-log-level
                  ["The log-level for text logging (only applicable when --json is not supplied, default: INFO)"
                   (format "Possible levels (in the ascending order): ~a"
                           (string-join (get-levels) ", "))]
                  (cond
                    [(member p-log-level (get-levels))
                     (set! arg-log-level p-log-level)]
                    [else (picus:user-error "unrecognized log-level: ~a" p-log-level)])]
 #:args (source)
 (cond
   [(or (circom-file? source) (r1cs-file? source)) source]
   [else (picus:user-error "file needs to have suffix .circom or .r1cs, got ~a" source)])))


(with-framework
  #:level (or arg-log-level "INFO")
  #:json? arg-json?
  #:truncate? (match arg-truncate?
                ['unset (not arg-json?)]
                [_ arg-truncate?])
  (λ ()

(unless (implies arg-opt-level (circom-file? source))
  (picus:user-error "--opt-level only applicable for circom source"))

(define opt-level (or arg-opt-level "0"))

(unless (implies arg-patch? (circom-file? source))
  (picus:user-error "--patch-circom only applicable for circom source"))

(unless (implies arg-log-level (not arg-json?))
  (picus:user-error "--log-level only applicable when --json is not given"))

(define (invoke-system cmd . args)
  (define outp (open-output-string))
  (define ret
    (parameterize ([current-output-port outp]
                   [current-error-port outp])
      (apply system* cmd args)))
  (values ret (get-output-string outp)))

;; compile-circom :: path? -> path?
;; compile circom file to r1cs file
(define (compile-circom circom-path)
  (define-values (ret out)
    (invoke-system (find-executable-path "circom")
                   "-o"
                   (get-tmpdir)
                   "--r1cs"
                   circom-path
                   "--sym"
                   (match opt-level
                     ["0" "--O0"]
                     ["1" "--O1"]
                     ["2" "--O2"])))
  (cond
    [ret (picus:log-debug "circom output: ~a" out)]
    [else
     (picus:log-error "[circom] ~a" out)
     (picus:user-error "circom compilation failed")])
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
    [(r1cs-file? source) (values source (r1cs:read-r1cs source))]
    [else (compile+patch-circom source arg-patch?)]))

(picus:log-debug "log level: ~a" arg-log-level)
(picus:log-debug "source format: ~a" (if (circom-file? source) "circom" "r1cs"))
(picus:log-debug "r1cs file: ~a" r1cs-path)
(picus:log-debug "timeout: ~a" arg-timeout)
(picus:log-debug "solver: ~a" arg-solver)
(picus:log-debug "selector: ~a" arg-selector)
(picus:log-debug "precondition: ~a" arg-precondition)
(picus:log-debug "propagation enabled: ~a" arg-prop)
(picus:log-debug "solver enabled: ~a" arg-slv)
(picus:log-debug "safety mode: ~a" (if arg-strong "strong" "weak"))

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
(picus:log-debug "number of wires: ~a" nwires)
(picus:log-debug "number of constraints: ~a" mconstraints)
(picus:log-debug "field size (how many bytes): ~a" (r1cs:get-field-size r0))
(picus:log-debug "prime number: ~a" (r1cs:get-prime-number r0))
(config:set-p! (r1cs:get-prime-number r0))

; categorize signals
(define input-list (r1cs:r1cs-inputs r0))
(define input-set (list->set input-list))
(define output-list (r1cs:r1cs-outputs r0))
(define output-set (list->set output-list))
(define target-set (if arg-strong (list->set (range nwires)) (list->set output-list)))
(picus:log-debug "inputs: ~e" input-list)
(picus:log-debug "outputs: ~e" output-list)
(picus:log-debug "targets: ~e" target-set)

; parse original r1cs
(picus:log-progress "parsing original r1cs...")
;; invariant: (length varlist) = nwires
(define-values (varlist opts defs cnsts) (parse-r1cs r0 '())) ; interpret the constraint system
(picus:log-debug "varlist: ~e" varlist)
; parse alternative r1cs
(define alt-varlist
  (for/list ([i (in-range nwires)] [var (in-list varlist)])
    (if (not (utils:contains? input-list i))
        (format "y~a" i)
        var)))
(picus:log-debug "alt-varlist ~e" alt-varlist)
(picus:log-progress "parsing alternative r1cs...")
(define-values (_ __ alt-defs alt-cnsts) (parse-r1cs r0 alt-varlist))

(picus:log-progress "configuring precondition...")
(define-values (unique-set precondition)
  (if (null? arg-precondition)
      (values (set) '())
      (pre:read-precondition arg-precondition))) ; read!
(picus:log-debug "unique: ~a" unique-set)

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
(define-values (res res-ks res-us readable-res-info raw-res-info)
  (dpvl:apply-algorithm
   r0 nwires mconstraints
   input-set output-set target-set
   varlist opts defs cnsts
   alt-varlist alt-defs alt-cnsts
   unique-set precondition ; prior knowledge row
   arg-selector arg-prop arg-slv arg-timeout path-sym
   solve interpret-r1cs
   optimize-r1cs-p0 expand-r1cs normalize-r1cs optimize-r1cs-p1))
(picus:log-debug "raw map: ~a" raw-res-info)
(picus:log-debug "final unknown set ~e" res-us)
(picus:log-debug "~a uniqueness: ~a" (if arg-strong "strong" "weak") res)

;; format-cex :: string?, (listof (pairof string? any/c)), #:diff (listof (pairof string? any/c)) -> void?
(define (format-cex heading info #:diff [diff info])
  (picus:log-main "  ~a:" heading)
  (for ([entry (in-list info)] [diff-entry (in-list diff)])
    (picus:log-main (cond
                      [(equal? (cdr entry) (cdr diff-entry))
                       "    ~a: ~a"]
                      [else (highlight "    ~a: ~a")])
                    (car entry) (cdr entry)))
  (when (empty? info)
    (picus:log-main "    no ~a" heading)))

(match res
  ['unsafe
   (picus:log-main "The circuit is underconstrained")
   (picus:log-main "Counterexample:")
   (match-define (list in out1 out2 other1 other2)
     readable-res-info)

   (format-cex "inputs" in)
   (format-cex "first possible outputs" out1 #:diff out2)
   (format-cex "second possible outputs" out2 #:diff out1)
   (format-cex "first internal variables" other1)
   (format-cex "second internal variables" other2)

   (when arg-wtns
     (parameterize ([current-directory arg-wtns])
       (gen-witness raw-res-info r0)))

   (picus:exit exit-code:issues)]
  ['safe
   (picus:log-main "The circuit is properly constrained")]
  ['unknown
   (picus:log-main "Cannot determine whether the circuit is properly constrained")])

(when arg-clean?
  (clean-tmpdir!))))
