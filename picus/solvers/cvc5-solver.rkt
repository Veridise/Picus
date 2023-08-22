#lang racket
(require racket/engine
    (prefix-in tokamak: "../tokamak.rkt")
    (prefix-in config: "../config.rkt")
)
(provide (all-defined-out))

; stateful variable that stores smt path
(define state-smt-path null)

; solving component
(define (solve smt-str timeout #:verbose? [verbose? #f] #:output-smt? [output-smt? #f])
    (define temp-folder (find-system-path 'temp-dir))
    (define temp-file (format "picus~a.smt2"
        (string-replace (format "~a" (current-inexact-milliseconds)) "." "")))
    (define temp-path (build-path temp-folder temp-file))
    (set! state-smt-path temp-path)
    (define smt-file (open-output-file temp-path))
    (display smt-str smt-file)
    (close-output-port smt-file)
    (when (or verbose? output-smt?)
        (printf "(written to: ~a)\n" temp-path)
    )

    (when verbose?
        (printf "# solving...\n")
    )
    (define-values (sp out in err)
            ; (note) use `apply` to expand the last argument
            ; (apply subprocess #f #f #f (find-executable-path "cvc5") (list temp-path))
            (apply subprocess #f #f #f (find-executable-path "cvc5") (list temp-path "--produce-models"))
    )
    (define engine0 (engine (lambda (_)
        (define out-str (port->string out))
        (define err-str (port->string err))
        (close-input-port out)
        (close-output-port in)
        (close-input-port err)
        (subprocess-wait sp)
        (cons out-str err-str)
    )))
    (define eres (engine-run timeout engine0))
    (define esol (engine-result engine0))
    (cond
        [(not eres)
            ; need to kill the process
            (subprocess-kill sp #t)
            (cons 'timeout "")
        ]
        [else
            (define out-str (car esol))
            (define err-str (cdr esol))
            (when verbose?
                (printf "# stdout:\n~a\n" out-str)
                (printf "# stderr:\n~a\n" err-str)
            )
            (cond
                [(non-empty-string? err-str) (cons 'error err-str)] ; something wrong, not solved
                [(string-prefix? out-str "unsat") (cons 'unsat out-str)]
                [(string-prefix? out-str "sat") (cons 'sat (parse-model out-str))]
                [(string-prefix? out-str "unknown") (cons 'unknown out-str)]
                [else (cons 'else out-str)]
            )
        ]
    )
)

(define readtable-for-parsing
  ;; Since 368f3c3, cvc5 can produce a finite field literal, in the format:
  ;;   #f <value> m <mod-value>
  ;; See https://github.com/cvc5/cvc5/commit/368f3c3ed695e925f0eea1b9d6a8280cdfa9f64c
  ;; Here, we simply want to extract the value.
  (make-readtable #f #\f 'dispatch-macro
                  (λ (_ port src line col pos)
                    (match (symbol->string (read port))
                      [(pregexp #px"^(\\d+)m\\d+$" (list _ (app string->number val)))
                       val]))))

; example string:
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
      [`(define-fun ,var () #;type ,_ ,val)
       ; update model
       (hash-set! model (symbol->string var)
                  (if (< val 0)
                      (+ config:p val) ; remap to field
                      val))]
      [_ (tokamak:exit "model parsing error, check: ~a" binding)]))
  ; return the model
  model)
