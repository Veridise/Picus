#lang racket

(require racket/cmdline
         racket/runtime-path
         racket/port
         racket/format
         racket/match
         rackunit

         "../picus/exit.rkt")

(provide make-run-config
         check-result
         env-cvc5-int
         env-cvc5-bitsum)

(define-runtime-path picus "../picus.rkt")
(define-runtime-path benchmark-dir "../benchmarks/")
(define-runtime-path solver-dir "../solvers/")

(define env-cvc5-int (environment-variables-copy (current-environment-variables)))
(define env-cvc5-bitsum (environment-variables-copy (current-environment-variables)))
(let ()
  (parameterize ([current-environment-variables env-cvc5-int])
    (putenv "SOLVER_PATH" (~a (build-path solver-dir "cvc5-int"))))
  (parameterize ([current-environment-variables env-cvc5-bitsum])
    (putenv "SOLVER_PATH" (~a (build-path solver-dir "cvc5-bitsum"))))
  (void))

(struct run-config (filename timeout) #:transparent)

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

  (define bench-path (~a (build-path benchmark-dir filename)))

  (parameterize ([current-namespace (make-base-namespace)]
                 [current-command-line-arguments
                  (vector "--log-level" "ACCOUNTING"
                          "--json" (~a bench-path ".json")
                          bench-path)]
                 [current-output-port out])
    (define (run-picus-thunk)
      (set! ret-code
            (with-handlers ([exit-code? exit-code-code])
              (parameterize ([exit-handler (λ (code) (raise (exit-code code)))])
                (dynamic-require (list 'submod picus 'main) #f)))))
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
    (define current-counter *counter*)
    (set! *counter* (add1 *counter*))
    (cond
      [(or (not *num-threads*) (= (modulo current-counter *num-threads*) *thread-id*))
       (check:core run-conf expected)]
      [else (printf "skipping a run for other threads\n")])))
