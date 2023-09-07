#lang racket/base

(provide make-solve)
(require racket/string
         racket/port
         racket/match
         racket/engine
         (prefix-in tokamak: "../tokamak.rkt")
         (prefix-in config: "../config.rkt"))

(define ((make-solve #:executable executable
                     #:options [options '()])
         smt-str timeout #:verbose? [verbose? #f] #:output-smt? [output-smt? #f])
  (define temp-folder (find-system-path 'temp-dir))
  (define temp-file (format "picus~a.smt2"
                            (string-replace (format "~a" (current-inexact-milliseconds)) "." "")))
  (define temp-path (build-path temp-folder temp-file))
  (with-output-to-file temp-path
    (λ () (display smt-str)))
  (when (or verbose? output-smt?)
    (printf "(written to: ~a)\n" temp-path))

  (when verbose?
    (printf "# solving...\n"))
  (define-values (sp out in err)
    ; (note) use `apply` to expand the last argument
    ; (apply subprocess #f #f #f (find-executable-path "cvc5") (list temp-path))
    (apply subprocess #f #f #f (find-executable-path executable) temp-path options))

  (close-output-port in)

  (define engine0
    (engine
     (lambda (_)
       (define out-str (port->string out #:close? #t))
       (define err-str (port->string err #:close? #t))
       (subprocess-wait sp)
       (cons out-str err-str))))

  (define eres
    (dynamic-wind
      void
      (λ () (engine-run timeout engine0))
      (λ ()
        (when (eq? 'running (subprocess-status sp))
          (subprocess-kill sp #t))
        (close-input-port out)
        (close-input-port err))))

  (cond
    [eres
     (match-define (cons out-str err-str) (engine-result engine0))
     (when verbose?
       (printf "# stdout:\n~a\n" out-str)
       (printf "# stderr:\n~a\n" err-str))
     (values (cond
               [(non-empty-string? err-str) (cons 'error err-str)] ; something wrong, not solved
               [(string-prefix? out-str "unsat") (cons 'unsat out-str)]
               [(string-prefix? out-str "sat") (cons 'sat (parse-model out-str))]
               [(string-prefix? out-str "unknown") (cons 'unknown out-str)]
               [else (cons 'else out-str)])
             temp-path)]
    [else (values (cons 'timeout "") temp-path)]))

; example cvc5 string:
; sat
; (
; (define-fun x0 () (_ FiniteField 21888242871839275222246405745257275088548364400416034343698204186575808495617) 0)
; (define-fun x1 () (_ FiniteField 21888242871839275222246405745257275088548364400416034343698204186575808495617) 0)
; (define-fun x2 () (_ FiniteField 21888242871839275222246405745257275088548364400416034343698204186575808495617) -1)
; (define-fun x3 () (_ FiniteField 21888242871839275222246405745257275088548364400416034343698204186575808495617) 0)
; (define-fun x4 () (_ FiniteField 21888242871839275222246405745257275088548364400416034343698204186575808495617) 0)
; (define-fun p () (_ FiniteField 21888242871839275222246405745257275088548364400416034343698204186575808495617) 0)
; (define-fun ps1 () (_ FiniteField 21888242871839275222246405745257275088548364400416034343698204186575808495617) -1)
; (define-fun ps2 () (_ FiniteField 21888242871839275222246405745257275088548364400416034343698204186575808495617) -2)
; (define-fun ps3 () (_ FiniteField 21888242871839275222246405745257275088548364400416034343698204186575808495617) -3)
; (define-fun ps4 () (_ FiniteField 21888242871839275222246405745257275088548364400416034343698204186575808495617) -4)
; (define-fun ps5 () (_ FiniteField 21888242871839275222246405745257275088548364400416034343698204186575808495617) -5)
; (define-fun zero () (_ FiniteField 21888242871839275222246405745257275088548364400416034343698204186575808495617) 0)
; (define-fun one () (_ FiniteField 21888242871839275222246405745257275088548364400416034343698204186575808495617) 1)
; (define-fun y1 () (_ FiniteField 21888242871839275222246405745257275088548364400416034343698204186575808495617) 1)
; (define-fun y2 () (_ FiniteField 21888242871839275222246405745257275088548364400416034343698204186575808495617) -1)
; )
;
; example z3 string:
;   sat
; (
;   (define-fun ps2 () Int
;     21888242871839275222246405745257275088548364400416034343698204186575808495615)
;   (define-fun x2 () Int
;     0)
;   (define-fun zero () Int
;     0)
;   (define-fun y1 () Int
;     1)
;   (define-fun ps3 () Int
;     21888242871839275222246405745257275088548364400416034343698204186575808495614)
;   (define-fun x3 () Int
;     0)
;   (define-fun x0 () Int
;     0)
;   (define-fun one () Int
;     1)
;   (define-fun p () Int
;     21888242871839275222246405745257275088548364400416034343698204186575808495617)
;   (define-fun x4 () Int
;     0)
;   (define-fun y2 () Int
;     0)
;   (define-fun y3 () Int
;     1)
;   (define-fun ps4 () Int
;     21888242871839275222246405745257275088548364400416034343698204186575808495613)
;   (define-fun x1 () Int
;     0)
;   (define-fun ps1 () Int
;     21888242871839275222246405745257275088548364400416034343698204186575808495616)
;   (define-fun ps5 () Int
;     21888242871839275222246405745257275088548364400416034343698204186575808495612)
; )
;
; example cvc4 string:
;  sat
; (model
; (define-fun x0 () Int 0)
; (define-fun x1 () Int 1)
; (define-fun x2 () Int 0)
; (define-fun x3 () Int 1)
; (define-fun x4 () Int 0)
; (define-fun p () Int 21888242871839275222246405745257275088548364400416034343698204186575808495617)
; (define-fun ps1 () Int 21888242871839275222246405745257275088548364400416034343698204186575808495616)
; (define-fun ps2 () Int 21888242871839275222246405745257275088548364400416034343698204186575808495615)
; (define-fun ps3 () Int 21888242871839275222246405745257275088548364400416034343698204186575808495614)
; (define-fun ps4 () Int 21888242871839275222246405745257275088548364400416034343698204186575808495613)
; (define-fun ps5 () Int 21888242871839275222246405745257275088548364400416034343698204186575808495612)
; (define-fun zero () Int 0)
; (define-fun one () Int 1)
; (define-fun y1 () Int 0)
; (define-fun y2 () Int 0)
; (define-fun y3 () Int 0)
; )

(define readtable-for-parsing
  ;; Since 368f3c3, cvc5 can produce a finite field literal, in the format:
  ;;   #f <value> m <mod-value>
  ;; See https://github.com/cvc5/cvc5/commit/368f3c3ed695e925f0eea1b9d6a8280cdfa9f64c
  ;; Here, we simply want to extract the value.
  (make-readtable #f #\f 'dispatch-macro
                  (λ (_ port src line col pos)
                    (match (symbol->string (read port))
                      ;; we have already consumed "#f" from the dispatch macro
                      ;; so here, we consume the rest of the token: <value> m <mod-value>
                      [(pregexp #px"^(\\d+)m\\d+$" (list _ (app string->number val)))
                       val]))))

(define (parse-model arg-str)
  (define port (open-input-string arg-str))
  ;; this consumes the sat token
  (read port)
  ;; this consumes ( (define-fun) ... )
  (define raw-model
    (parameterize ([current-readtable readtable-for-parsing])
      (read port)))
  (define model (make-hash))
  (for ([binding (in-list raw-model)])
    (match binding
      ;; check that val is a number so that non-number will get reported below.
      [`(define-fun ,var () #;type ,_ ,(? number? val))
       ; update model
       (hash-set! model (symbol->string var)
                  (if (< val 0)
                      (+ config:p val) ; remap to field
                      val))]
      [_ (tokamak:exit "model parsing error, check: ~a" binding)]))
  ; return the model
  model)
