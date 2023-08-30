#lang racket/base

(module+ test
  (require rackunit
           racket/runtime-path
           racket/port
           racket/format
           racket/match)

  (define-runtime-path picus "../picus.rkt")
  (define-runtime-path benchmark-dir "../benchmarks/circomlib-cff5ab6/")

  (define-check (check-result filename expected)
    (printf "=== checking ~a ===\n" filename)
    (define-values (in out) (make-pipe))
    (define orig-out (current-output-port))
    (define string-port (open-output-string))
    (parameterize ([current-namespace (make-base-namespace)]
                   [current-command-line-arguments
                    (vector "--solver" "cvc5"
                            "--timeout" "5000"
                            "--weak"
                            "--verbose" "1"
                            "--r1cs" (~a (build-path benchmark-dir
                                                     (format "~a.r1cs" filename))))]
                   [current-output-port out])
      (define thd (thread (λ () (copy-port in orig-out string-port))))
      (time (dynamic-require picus #f))
      (close-output-port out)
      (thread-wait thd))
    (check-regexp-match
     (match expected
       ['safe #px"(?m:^# weak uniqueness: safe\\.$)"]
       ['unsafe #px"(?m:^# weak uniqueness: unsafe\\.$)"]
       ['unknown #px"(?m:^# weak uniqueness: unknown\\.$)"])
     (get-output-string string-port)))

  (check-result "AND@gates" 'safe)
  (check-result "BabyAdd@babyjub" 'unknown)
  (check-result "BabyDbl@babyjub" 'safe)

  ;; NOTE: BabyPbk@babyjub timeouted
  #;(check-result "BabyPbk@babyjub" '???)

  (check-result "BinSub@binsub" 'safe)
  (check-result "BinSum@binsum" 'safe)

  ;; NOTE: BitElementMulAny@escalarmulany was unsafe,
  ;; but the basis lemma fix made it unknown
  (check-result "BitElementMulAny@escalarmulany" 'unknown)

  (check-result "Bits2Num_strict@bitify" 'safe)
  (check-result "Bits2Num@bitify" 'safe)

  ;; NOTE: Bits2Point_Strict@pointbits timeouted
  #;(check-result "Bits2Point_Strict@pointbits" '???)

  (check-result "CompConstant@compconstant" 'safe)
  (check-result "Decoder@multiplexer" 'unsafe)
  (check-result "Edwards2Montgomery@montgomery" 'unsafe)

  ;; NOTE: EscalarMulAny@escalarmulany is technically unknown,
  ;; but it takes 100s to run, so don't run it here.
  #;(check-result "EscalarMulAny@escalarmulany" 'unknown)

  (check-result "EscalarProduct@multiplexer" 'safe)
  (check-result "GreaterEqThan@comparators" 'safe)
  (check-result "GreaterThan@comparators" 'safe)
  (check-result "IsEqual@comparators" 'safe)
  (check-result "IsZero@comparators" 'safe)
  (check-result "LessEqThan@comparators" 'safe)
  (check-result "LessThan@comparators" 'safe)
  (check-result "MiMC7@mimc" 'safe)
  (check-result "MiMCFeistel@mimcsponge" 'safe)
  (check-result "MiMCSponge@mimcsponge" 'safe)
  (check-result "Montgomery2Edwards@montgomery" 'unsafe)
  (check-result "MontgomeryAdd@montgomery" 'unsafe)
  (check-result "MontgomeryDouble@montgomery" 'unsafe)
  (check-result "MultiAND@gates" 'safe)
  (check-result "MultiMiMC7@mimc" 'safe)
  (check-result "MultiMux1@mux1" 'safe)
  (check-result "MultiMux2@mux2" 'safe)
  (check-result "MultiMux3@mux3" 'safe)
  (check-result "MultiMux4@mux4" 'safe)
  (check-result "Multiplexer@multiplexer" 'safe)
  (check-result "Multiplexor2@escalarmulany" 'safe)
  (check-result "Mux1@mux1" 'safe)
  (check-result "Mux2@mux2" 'safe)
  (check-result "Mux3@mux3" 'safe)
  (check-result "Mux4@mux4" 'safe)
  (check-result "NAND@gates" 'safe)
  (check-result "NOR@gates" 'safe)
  (check-result "NOT@gates" 'safe)

  ;; NOTE: Num2Bits_strict@bitify was safe,
  ;; but the basis lemma fix made it timeouted
  #;(check-result "Num2Bits_strict@bitify" 'safe)

  (check-result "Num2Bits@bitify" 'safe)
  (check-result "Num2BitsNeg@bitify" 'safe)
  (check-result "OR@gates" 'safe)
  (check-result "Pedersen@pedersen_old" 'safe)
  (check-result "Pedersen@pedersen" 'unknown)

  ;; NOTE: Point2Bits_Strict@pointbits was safe,
  ;; but the basis lemma fix made it timeouted
  #;(check-result "Point2Bits_Strict@pointbits" 'safe)

  (check-result "Poseidon@poseidon" 'safe)

  ;; NOTE: Segment@pedersen, SegmentMulAny@escalarmulany, SegmentMulFix@escalarmulfix timeouted
  #;(check-result "Segment@pedersen" '???)
  #;(check-result "SegmentMulAny@escalarmulany" '???)
  #;(check-result "SegmentMulFix@escalarmulfix" '???)

  (check-result "Sigma@poseidon" 'safe)
  (check-result "Sign@sign" 'safe)
  (check-result "Switcher@switcher" 'safe)

  ;; NOTE: Window4@pedersen, WindowMulFix@escalarmulfix timeouted
  #;(check-result "Window4@pedersen" '???)
  #;(check-result "WindowMulFix@escalarmulfix" '???)

  (check-result "XOR@gates" 'safe))
