#lang racket
(require racket/engine
    (prefix-in tokamak: "../tokamak.rkt")
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
        (printf "# written to: ~a\n" temp-path)
    )

    (when verbose?
        (printf "# solving...\n")
    )
    (define-values (sp out in err)
            ; (note) use `apply` to expand the last argument
            ; (apply subprocess #f #f #f (find-executable-path "cvc4") (list temp-path))
            (apply subprocess #f #f #f (find-executable-path "cvc4") (list temp-path "--produce-models"))
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
            (close-input-port out)
            (close-output-port in)
            (close-input-port err)
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

; example string:
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
; .
(define (parse-model arg-str)
    (define strlist (string-split arg-str "\n"))
    (define model (make-hash))
    (for ([s strlist])
        (define res (regexp-match* #rx"define-fun (.*?) .* (.*?)\\)" s #:match-select cdr))
        (when (not (empty? res))
            (define var (list-ref (list-ref res 0) 0))
            (define val (string->number (list-ref (list-ref res 0) 1)))
            ; not a number
            (when (boolean? val) (tokamak:exit "model parsing error, check: ~a" s))
            ; update model
            (hash-set! model var val)
        )
    )
    ; return the model
    model
)
