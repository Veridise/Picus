#lang racket/base

(module+ test
  (require racket/system
           racket/cmdline
           racket/string
           racket/file
           racket/runtime-path
           racket/port
           racket/format
           racket/match
           rackunit
           "../picus/public-inputs.rkt")

  (define filenames
    (command-line
     #:args filenames
     filenames))

  (define-runtime-path picus "../picus.rkt")
  (define-runtime-path benchmark-dir "../benchmarks/circomlib-cff5ab6/")

  (struct run-config (filename timeout) #:transparent)

  (define (make-run-config filename #:timeout [timeout 60])
    (run-config filename timeout))

  (define (get-full-path filename)
    (~a (build-path benchmark-dir filename)))

  (define (flush)
    (flush-output)
    (flush-output (current-error-port)))

  (define (compile-circom circom-path)
    (flush)
    (system* (find-executable-path "circom")
             "-o"
             benchmark-dir
             "--r1cs"
             circom-path
             "--sym"
             "--O0"
             #;"--jsons")
    (flush))

  (define (has-public-input? orig-content)
    ;; match a line starting with "component main =".
    ;; This is very hacky, but it is the case that in our benchmarks,
    ;; every file that contains "component main =" has no public input.
    (not (regexp-match? #px"(?m:^component main =)" orig-content)))

  (define-check (check:core run-conf expected)
    (match-define (run-config filename timeout) run-conf)
    (printf "=== checking ~a ===\n" filename)
    (printf "##[group]~a\n" filename)
    (printf "Compiling circom file\n")
    (define circom-path (get-full-path (format "~a.circom" filename)))
    (define r1cs-path (get-full-path (format "~a.r1cs" filename)))
    (define sym-path (get-full-path (format "~a.sym" filename)))

    ;; initially compile Circom to get information about public inputs
    (compile-circom circom-path)

    (define orig-content (file->string circom-path))

    (define real-r1cs-path
      (cond
        [(has-public-input? orig-content) r1cs-path]
        [else
         (define public-inputs (get-public-inputs r1cs-path sym-path))
         (define patched-circom-path (get-full-path (format "patched-~a.circom" filename)))
         (define patched-r1cs-path (get-full-path (format "patched-~a.r1cs" filename)))

         (with-output-to-file patched-circom-path
           #:exists 'replace
           (λ ()
             (displayln
              (string-replace
               orig-content
               "component main ="
               (format "component main {public [~a]} ="
                       (string-join public-inputs ", "))))))

         (compile-circom patched-circom-path)
         patched-r1cs-path]))

    (printf "Starting Picus\n")
    (define-values (in out) (make-pipe))
    (define orig-out (current-output-port))
    (define string-port (open-output-string))
    (define pump-thd (thread (λ () (copy-port in orig-out string-port))))

    (define timing-info #f)

    (parameterize ([current-namespace (make-base-namespace)]
                   [current-command-line-arguments
                    (vector "--solver" "cvc5"
                            "--timeout" "5000"
                            "--weak"
                            "--verbose" "1"
                            "--r1cs" real-r1cs-path)]
                   [current-output-port out])
      (define main-thd
        (thread (λ ()
                  (match-define-values (_ cpu real gc)
                    (time-apply (λ () (dynamic-require picus #f)) '()))
                  (set! timing-info (list cpu real gc)))))
      (match (sync/timeout/enable-break timeout main-thd)
        [#f
         ;; this means we timeouted
         (break-thread main-thd)
         (set! timing-info #f)]
        [_
         ;; we run to completion
         (void)]))
    (close-output-port out)
    (thread-wait pump-thd)
    (flush)
    (printf "##[endgroup]\n")
    (match timing-info
      [(list cpu real gc)
       (printf "cpu: ~a; real: ~a; gc: ~a\n" cpu real gc)]
      [_ (printf "TIMEOUT\n")])

    (match expected
      [(or 'safe 'unsafe 'unknown)
       (check-regexp-match
        (match expected
          ['safe #px"(?m:^# weak uniqueness: safe\\.$)"]
          ['unsafe #px"(?m:^# weak uniqueness: unsafe\\.$)"]
          ['unknown #px"(?m:^# weak uniqueness: unknown\\.$)"])
        (get-output-string string-port))]
      ['timeout
       (check-false timing-info)])
    (flush))

  (define-check (check-result run-conf expected)
    (match-define (run-config filename _) run-conf)
    (when (or (member filename filenames) (null? filenames))
      (check:core run-conf expected)))

  (check-result (make-run-config "AND@gates") 'safe)
  (check-result (make-run-config "BabyAdd@babyjub") 'unknown)
  (check-result (make-run-config "BabyDbl@babyjub") 'safe)
  (check-result (make-run-config "BabyPbk@babyjub") 'timeout)
  (check-result (make-run-config "BinSub@binsub") 'safe)
  (check-result (make-run-config "BinSum@binsum") 'safe)

  ;; NOTE: was unsafe, but the basis lemma fix made it unknown
  (check-result (make-run-config "BitElementMulAny@escalarmulany" #:timeout 200) 'unknown)

  (check-result (make-run-config "Bits2Num_strict@bitify") 'safe)
  (check-result (make-run-config "Bits2Num@bitify") 'safe)
  (check-result (make-run-config "Bits2Point_Strict@pointbits") 'timeout)
  (check-result (make-run-config "CompConstant@compconstant") 'safe)
  (check-result (make-run-config "Decoder@multiplexer") 'unsafe)
  (check-result (make-run-config "Edwards2Montgomery@montgomery") 'unsafe)

  ;; NOTE: is actually unknown, but it takes long time to run (~100s)
  (check-result (make-run-config "EscalarMulAny@escalarmulany") 'timeout)

  (check-result (make-run-config "EscalarProduct@multiplexer") 'safe)
  (check-result (make-run-config "GreaterEqThan@comparators") 'safe)
  (check-result (make-run-config "GreaterThan@comparators") 'safe)
  (check-result (make-run-config "IsEqual@comparators") 'safe)
  (check-result (make-run-config "IsZero@comparators") 'safe)
  (check-result (make-run-config "LessEqThan@comparators") 'safe)
  (check-result (make-run-config "LessThan@comparators") 'safe)
  (check-result (make-run-config "MiMC7@mimc") 'safe)
  (check-result (make-run-config "MiMCFeistel@mimcsponge") 'safe)
  (check-result (make-run-config "MiMCSponge@mimcsponge") 'safe)
  (check-result (make-run-config "Montgomery2Edwards@montgomery") 'unsafe)
  (check-result (make-run-config "MontgomeryAdd@montgomery") 'unsafe)
  (check-result (make-run-config "MontgomeryDouble@montgomery") 'unsafe)
  (check-result (make-run-config "MultiAND@gates") 'safe)
  (check-result (make-run-config "MultiMiMC7@mimc") 'safe)
  (check-result (make-run-config "MultiMux1@mux1") 'safe)
  (check-result (make-run-config "MultiMux2@mux2") 'safe)
  (check-result (make-run-config "MultiMux3@mux3") 'safe)
  (check-result (make-run-config "MultiMux4@mux4") 'safe)
  (check-result (make-run-config "Multiplexer@multiplexer") 'safe)
  (check-result (make-run-config "Multiplexor2@escalarmulany") 'safe)
  (check-result (make-run-config "Mux1@mux1") 'safe)
  (check-result (make-run-config "Mux2@mux2") 'safe)
  (check-result (make-run-config "Mux3@mux3") 'safe)
  (check-result (make-run-config "Mux4@mux4") 'safe)
  (check-result (make-run-config "NAND@gates") 'safe)
  (check-result (make-run-config "NOR@gates") 'safe)
  (check-result (make-run-config "NOT@gates") 'safe)

  ;; was safe, but the basis lemma fix made it timeouted
  (check-result (make-run-config "Num2Bits_strict@bitify") 'timeout)

  (check-result (make-run-config "Num2Bits@bitify") 'safe)
  (check-result (make-run-config "Num2BitsNeg@bitify") 'safe)
  (check-result (make-run-config "OR@gates") 'safe)
  (check-result (make-run-config "Pedersen@pedersen_old") 'safe)
  (check-result (make-run-config "Pedersen@pedersen" #:timeout 200) 'unknown)

  ;; was safe, but the basis lemma fix made it timeouted
  (check-result (make-run-config "Point2Bits_Strict@pointbits") 'timeout)

  (check-result (make-run-config "Poseidon@poseidon") 'safe)
  (check-result (make-run-config "Segment@pedersen") 'timeout)
  (check-result (make-run-config "SegmentMulAny@escalarmulany") 'timeout)
  (check-result (make-run-config "SegmentMulFix@escalarmulfix") 'timeout)
  (check-result (make-run-config "Sigma@poseidon") 'safe)
  (check-result (make-run-config "Sign@sign") 'safe)
  (check-result (make-run-config "Switcher@switcher") 'safe)
  (check-result (make-run-config "Window4@pedersen") 'timeout)
  (check-result (make-run-config "WindowMulFix@escalarmulfix") 'timeout)
  (check-result (make-run-config "XOR@gates") 'safe))
