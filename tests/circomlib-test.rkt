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

  ;; NOTE: BabyDbl@babyjub timeouted
  #;(check-result "BabyDbl@babyjub" '???)

  (check-result "BinSub@binsub" 'safe)
  (check-result "BinSum@binsum" 'safe)

  ;; NOTE: BitElementMulAny@escalarmulany was unsafe,
  ;; but the basis lemma fix made it unknown
  (check-result "BitElementMulAny@escalarmulany" 'unknown)

  ;; NOTE: BitElementMulAny@escalarmulany was safe,
  ;; but the basis lemma fix made it timeouted
  #;(check-result "Bits2Num_strict@bitify" 'safe)

  (check-result "Bits2Num@bitify" 'safe)

  ;; NOTE: Bits2Point_Strict@pointbits timeouted
  #;(check-result "Bits2Point_Strict@pointbits" '???)

  ;; NOTE: CompConstant@compconstant was safe,
  ;; but the basis lemma fix made it timeouted
  #;(check-result "CompConstant@compconstant" 'safe)

  (check-result "Decoder@multiplexer" 'unsafe)
  (check-result "Edwards2Montgomery@montgomery" 'unsafe)

  ;; NOTE: EscalarMulAny@escalarmulany is technically unknown,
  ;; but it takes 100s to run, so don't run it here.
  #;(check-result "EscalarMulAny@escalarmulany" 'unknown)

  (check-result "EscalarProduct@multiplexer" 'safe))
