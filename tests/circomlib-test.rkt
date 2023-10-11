#lang racket/base

(module+ test
  (require racket/cmdline
           racket/runtime-path
           racket/port
           racket/format
           racket/match
           rackunit

           "../picus/exit.rkt")

  ;; invariant: *thread-id* is false iff *num-threads* is false
  (define *thread-id* #f)
  (define *num-threads* #f)

  (define filenames
    (command-line
     #:once-each
     [("--parallel") thread-id num-threads
                     "Run the (thread-id)th batch of num-threads workload"
                     (set! *thread-id* (string->number thread-id))
                     (set! *num-threads* (string->number num-threads))]
     #:args filenames
     filenames))

  (define-runtime-path picus "../picus.rkt")
  (define-runtime-path benchmark-dir "../benchmarks/circomlib-cff5ab6/")

  (struct run-config (filename timeout) #:transparent)

  (define (make-run-config filename #:timeout [timeout 60])
    (run-config filename timeout))

  (define (flush)
    (flush-output)
    (flush-output (current-error-port)))

  (define-check (check:core run-conf expected)
    (match-define (run-config filename timeout) run-conf)
    (printf "=== checking ~a ===\n" filename)
    (printf "##[group]~a\n" filename)
    (flush)
    (define-values (in out) (make-pipe))
    (define orig-out (current-output-port))
    (define string-port (open-output-string))
    (define pump-thd (thread (λ () (copy-port in orig-out string-port))))

    (define timing-info #f)
    (define ret-code #f)

    (struct exit-code (code) #:transparent)

    (parameterize ([current-namespace (make-base-namespace)]
                   [current-command-line-arguments
                    (vector "--solver" "cvc5"
                            "--timeout" "5000"
                            "--patch-circom"
                            "--wtns" "."
                            (~a (build-path
                                 benchmark-dir
                                 (format "~a.circom" filename))))]
                   [current-output-port out])
      (define (run-picus-thunk)
        (set! ret-code
              (with-handlers ([exit-code? exit-code-code])
                (parameterize ([exit-handler (λ (code) (raise (exit-code code)))])
                  (dynamic-require picus #f)))))
      (define main-thd
        (thread (λ ()
                  (match-define-values (_ cpu real gc)
                    (time-apply run-picus-thunk '()))
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
      ['safe (check-equal? ret-code exit-code:safe)]
      ['unknown (check-equal? ret-code exit-code:unknown)]
      ['unsafe (check-equal? ret-code exit-code:unsafe)]
      ['timeout (check-false ret-code)])

    (match expected
      [(or 'safe 'unsafe 'unknown)
       (check-regexp-match
        (match expected
          ['safe #px"(?m:^The circuit is properly constrained$)"]
          ['unsafe #px"(?m:^The circuit is underconstrained$)"]
          ['unknown #px"(?m:^Cannot determine whether the circuit is properly constrained$)"])
        (get-output-string string-port))
       (check-not-false timing-info)]
      ['timeout
       (check-false timing-info)])
    (flush))

  (define *counter* 0)
  (define-check (check-result run-conf expected)
    (match-define (run-config filename _) run-conf)
    (when (or (member filename filenames) (null? filenames))
      (cond
        [(or (not *num-threads*) (= (modulo *counter* *num-threads*) *thread-id*))
         (check:core run-conf expected)]
        [else (printf "skipping a run for other threads\n")])
      (set! *counter* (add1 *counter*))))

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
