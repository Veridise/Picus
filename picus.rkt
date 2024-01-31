#lang racket
; common require
(require racket/runtime-path
         (prefix-in config: "./picus/config.rkt")
         (prefix-in dpvl: "./picus/algorithms/dpvl.rkt")
         (prefix-in pre: "./picus/precondition.rkt")
         "picus/tmpdir.rkt"
         "picus/ansi.rkt"
         "picus/logging.rkt"
         "picus/framework.rkt"
         "picus/exit.rkt"
         "picus/subst.rkt")

(define-runtime-path selector-path "picus/algorithms/selector.rkt")
(define-runtime-path reader-path "picus/reader.rkt")
(define-runtime-path solver-path "picus/solver.rkt")

; =====================================
; ======== commandline parsing ========
; =====================================
; parse command line arguments
(define arg-json-target #f)
(define arg-truncate? #t)
(define arg-opt-level #f)
(define arg-clean? #t)
(define arg-timeout 5000)
(define arg-solver (dpvl:current-solver))
(define arg-selector (dpvl:current-selector))
(define arg-precondition null)
(define arg-prop #t)
(define arg-slv #t)
(define arg-strong #f)
(define arg-log-level #f)
(define arg-wtns #f)

(define (get-exports path)
  (define-values (exports _) (module->exports path))
  (for/list ([export (in-list (dict-ref exports 0 '()))])
    (symbol->string (first export))))

(define (load-from-module path name fmt)
  (dynamic-require
   path (string->symbol name)
   (λ ()
     (picus:user-error fmt
                       (string-join (get-exports path) " | ")
                       name))))

(define (gen-help-from-module path fmt def)
  (format fmt (string-join (get-exports path) " | ") def))

(define (extract-extension path)
  (match (path-get-extension path)
    [#f "no extension"]
    [bs (substring (bytes->string/utf-8 bs) 1)]))

(define reader
  (command-line
   #:program "run-picus"
   #:usage-help "<source> must be a file with .circom, .r1cs, or .sr1cs extension"
   #:once-each
   [("--json") json-target
               ["either:"
                "  - json logging output path; or"
                "  - '-', which suppresses the text logging mode and"
                "    outputs json logging to standard output"
                "(default: no json output)"]
               (set! arg-json-target json-target)]
   [("--noclean") "do not clean up temporary files (default: false)"
                  (set! arg-clean? #f)]
   [("--timeout") timeout "timeout for SMT query (default: 5000ms)"
                  (set! arg-timeout (string->number timeout))]
   [("--solver") solver
                 [(gen-help-from-module solver-path
                                        "solver to use: ~a (default: ~a)"
                                        (send arg-solver get-name))]
                 (set! arg-solver (load-from-module solver-path solver "valid solver: ~a, got ~a"))]
   [("--selector") selector
                   [(gen-help-from-module selector-path
                                          "selector to use: ~a (default: ~a)"
                                          (send arg-selector get-name))]
                   (set! arg-selector (load-from-module selector-path selector "valid selector: ~a, got ~a"))]
   [("--precondition") precondition "path to precondition json (default: none)"
                       (set! arg-precondition precondition)]
   [("--noprop") "disable propagation (default: false / propagation on)"
                 (set! arg-prop #f)]
   [("--nosolve") "disable solver phase (default: false / solver on)"
                  (set! arg-slv #f)]
   [("--strong") "check for strong safety (default: false)"
                 (set! arg-strong #t)]
   [("--truncate") truncate
                   "truncate overly long logged message: on | off (default: on)"
                   (match truncate
                     ["on" (set! arg-truncate? #t)]
                     ["off" (set! arg-truncate? #f)]
                     [_ (picus:user-error "unrecognized truncate mode: ~a" truncate)])]
   [("--log-level") log-level
                    ["The log-level for text logging (default: INFO)"
                     (format "Possible levels (in the ascending order): ~a"
                             (string-join (get-levels) ", "))]
                    (cond
                      [(member log-level (get-levels))
                       (set! arg-log-level log-level)]
                      [else (picus:user-error "unrecognized log-level: ~a" log-level)])]

   #:help-labels
   ""
   "circom options (only applicable for circom source)"
   ""
   #:once-each
   [("--opt-level") opt-level "optimization level for circom compilation (default: 0)"
                    (set! arg-opt-level
                          (match opt-level
                            [(or "0" "1" "2") opt-level]
                            [_ (picus:user-error "unrecognized optimization level: ~a" opt-level)]))]

   #:help-labels
   ""
   "circom and r1cs options (only applicable for circom and r1cs source)"
   ""
   #:once-each
   [("--wtns") wtns
               "wtns files output directory (default: don't output)"
               (set! arg-wtns wtns)]

   #:help-labels
   ""
   "other options"
   ""
   #:args (source)
   ((load-from-module reader-path
                      (extract-extension source)
                      "valid file extension: ~a, got ~a")
    source)))

(define (main)
  (define r0 (reader #:opt-level arg-opt-level))
  (send r0 validate arg-wtns)

  (picus:log-debug "log level: ~a" arg-log-level)
  (picus:log-debug "source format: ~a" (send r0 get-format))
  (picus:log-debug "timeout: ~a" arg-timeout)
  (picus:log-debug "solver: ~a" (send arg-solver get-name))
  (picus:log-debug "selector: ~a" (send arg-selector get-name))
  (picus:log-debug "precondition: ~a" arg-precondition)
  (picus:log-debug "propagation enabled: ~a" arg-prop)
  (picus:log-debug "solver enabled: ~a" arg-slv)
  (picus:log-debug "safety mode: ~a" (if arg-strong "strong" "weak"))

  ; ==================================
  ; ======== main preparation ========
  ; ==================================
  (picus:log-debug "number of wires: ~a" (send r0 get-num-wires))
  (picus:log-debug "number of constraints: ~a" (send r0 get-num-constraints))
  (picus:log-debug "prime number: ~a" (send r0 get-prime-number))
  (config:set-p! (send r0 get-prime-number))

  ; categorize signals
  (define input-list (send r0 get-top-level-inputs))
  (define input-set (list->set input-list))
  (define output-list (send r0 get-top-level-outputs))
  (define output-set (list->set output-list))
  (define target-set (if arg-strong
                         (for/set ([i (in-range (send r0 get-num-wires))]) i)
                         (list->set output-list)))
  (picus:log-debug "inputs: ~e" input-list)
  (picus:log-debug "outputs: ~e" output-list)
  (picus:log-debug "targets: ~e" target-set)

  ; parse original r1cs
  (picus:log-progress "parsing original r1cs...")
  ;; invariant: (length varlist) = nwires
  (define-values (varlist defs cnsts) (send arg-solver parse-r1cs r0 "x")) ; interpret the constraint system
  (picus:log-debug "varlist: ~e" varlist)
  ; parse alternative r1cs
  (picus:log-progress "parsing alternative r1cs...")
  (define-values (alt-varlist alt-defs alt-cnsts) (send arg-solver parse-r1cs r0 "y"))
  (picus:log-debug "alt-varlist ~e" alt-varlist)

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
  (picus:log-accounting #:type "started_algorithm")
  (match-define-values ((list res res-ks res-us readable-res-info raw-res-info) cpu real gc)
    (parameterize ([dpvl:current-selector arg-selector]
                   [dpvl:current-solver arg-solver])
      (time-apply (λ ()
                    (dpvl:apply-algorithm
                     r0
                     input-set output-set target-set
                     varlist (send arg-solver get-options) defs cnsts
                     alt-varlist alt-defs alt-cnsts
                     unique-set
                     (append precondition ; prior knowledge row
                             (map (λ (cnst) (cons "x series" cnst)) (send r0 get-extra-constraints))
                             (map (λ (cnst) (cons "y series" cnst)) (subst-vars* (send r0 get-extra-constraints) convert-y)))
                     arg-prop arg-slv arg-timeout))
                  '())))

  (picus:log-accounting #:type "finished_algorithm")
  (picus:log-accounting #:type "algorithm_time_cpu"
                        #:value cpu
                        #:unit "ms"
                        #:msg "Time spent for main algorithm (cpu)")
  (picus:log-accounting #:type "algorithm_time_real"
                        #:value real
                        #:unit "ms"
                        #:msg "Time spent for main algorithm (real)")
  (picus:log-accounting #:type "algorithm_time_gc"
                        #:value gc
                        #:unit "ms"
                        #:msg "Time spent for main algorithm (gc)")
  (picus:log-debug "raw map: ~a" raw-res-info)
  (picus:log-debug "final known set ~e" res-ks)
  (picus:log-debug "final unknown set ~e" res-us)
  (picus:log-debug "~a uniqueness: ~a" (if arg-strong "strong" "weak") res)
  (picus:log-accounting #:type "known_size"
                        #:value (set-count res-ks)
                        #:msg "Number of inferred known signals")

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

  (when arg-clean?
    (clean-tmpdir!))

  (match res
    ['unsafe
     (picus:log-accounting #:type "finished_with_cex")
     (picus:log-main "The circuit is underconstrained")
     (picus:log-main "Counterexample:")
     (match-define (list in out1 out2 other1 other2)
       readable-res-info)

     (format-cex "inputs" in)
     (format-cex "first possible outputs" out1 #:diff out2)
     (format-cex "second possible outputs" out2 #:diff out1)
     (format-cex "first internal variables" other1 #:diff other2)
     (format-cex "second internal variables" other2 #:diff other1)

     (when arg-wtns
       (parameterize ([current-directory arg-wtns])
         (send r0 gen-witness-files raw-res-info)))
     (picus:exit exit-code:unsafe)]
    ['safe
     (picus:log-accounting #:type "finished_with_guarantee")
     (picus:log-main "The circuit is properly constrained")
     (picus:exit exit-code:safe)]
    ['unknown
     (picus:log-accounting #:type "finished_wo_cex")
     (picus:log-main "Cannot determine whether the circuit is properly constrained")
     (picus:exit exit-code:unknown)]))

(module+ main
  (with-framework
    #:level (or arg-log-level "INFO")
    #:json-target arg-json-target
    #:truncate? arg-truncate?
    main))
