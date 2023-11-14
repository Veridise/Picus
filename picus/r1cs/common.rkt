#lang racket/base

(provide interp
         format-op
         emit)

(require racket/match
         syntax/parse/define
         racket/string
         "../exit.rkt"
         (prefix-in r1cs: "./r1cs-grammar.rkt")
         (for-syntax racket/base))

(define (format-op op proc args)
  (match args
    ['() (picus:tool-error "empty operation: ~a" op)]
    [(list x) (proc x)]
    [(cons x xs)
     (display "(")
     (display op)
     (display " ")
     (proc x)
     (display " ")
     (format-op op proc xs)
     (display ")")]))

(begin-for-syntax
  (define-syntax-class item
    (pattern x:string #:with gen #'(display x))
    (pattern x #:with gen #'x)))

;; (emit X ...) prints in order, where an element could be either
;; a string literal or a function call to do further printing.
(define-syntax-parse-rule (emit xs:item ...)
  (begin xs.gen ...))

(define (interp arg-r1cs prnt)
  (define p (open-output-string))
  (parameterize ([current-output-port p])
    (let loop ([arg-r1cs arg-r1cs])
      (prnt
       arg-r1cs
       (λ (arg-r1cs)
         (match arg-r1cs

           ; command level
           [(r1cs:rcmds vs)
            (for ([v vs])
              (loop v)
              (newline))]

           [(r1cs:rraw v) (display v)]
           [(r1cs:rlogic v) (printf "(set-logic ~a)" v)]
           [(r1cs:rdef v t) (emit "(declare-const " (loop v) " " (loop t) ")")]
           [(r1cs:rassert v) (emit "(assert " (loop v) ")")]
           [(r1cs:rcmt v) (printf "; ~a" v)]
           [(r1cs:rsolve ) (display "(check-sat)\n(get-model)")]

           ; sub-command level
           [(r1cs:req lhs rhs) (emit "(= " (loop lhs) " " (loop rhs) ")")]
           [(r1cs:rneq lhs rhs) (emit "(not (= " (loop lhs) " " (loop rhs) "))")]

           [(r1cs:rand vs) (format-op "and" loop vs)]
           [(r1cs:ror vs) (format-op "or" loop vs)]
           [(r1cs:rimp lhs rhs) (emit "(=> " (loop lhs) " " (loop rhs) ")")]

           [(r1cs:rvar v) (display v)]
           [(r1cs:rtype v) (display v)]
           [_ (picus:tool-error "not supported: ~a" arg-r1cs)])))))
  (get-output-string p))
