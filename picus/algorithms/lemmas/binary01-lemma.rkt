#lang racket
; this implements the binary lemma (similar to Ecne Rule 2a):
;   if (x-a)*(x-b)=0, then x \in {a,b}; namely if {a,b}={0,1}, then x \in {0,1}
; this requires p1cnsts
; (note) this lemma currently only applies to {a,b}={0,1}, add support for other values if necessary later
; (note) this lemma requires ab0 optimization first, applies on p1cnsts
(require (prefix-in r1cs: "../../r1cs/r1cs-grammar.rkt")
         "../../logging.rkt"
         "../../exit.rkt")
(provide apply-lemma)

; recursively apply linear lemma
(define (apply-lemma ks us p1cnsts range-vec)
    (picus:log-progress "[binary01 lemma] starting propagation")

    (process p1cnsts range-vec)

    ; for signals with only 1 value, they are already unique
    ; (note) but we also need to check for signals that have no valid values, which may be compilation error
    (define-values (new-ks new-us)
      (for/fold ([ks ks] [us us])
                ([rng (in-vector range-vec)]
                 [sid (in-naturals)]
                 #:when (set? rng))
        (match (set-count rng)
          [0
           (picus:tool-error "[binary01 lemma] range-vec has 0 candidate values, got ~a for signal ~a" rng sid)]
          ; (fixme) is this valid?
          [1
           ; good, this is unique
           (values (set-add ks sid) (set-remove us sid))]
          ; else do nothing
          [_ (values ks us)])))
    (let ([s0 (set-subtract new-ks ks)])
        (if (set-empty? s0)
            (picus:log-debug "[binary01 lemma] nothing added")
            (picus:log-debug "[binary01 lemma] adding ~e" s0)))
    ; apply once is enough, return
    (values new-ks new-us)
)

(define (override!-range range-vec sid rng)
    (cond
        [(equal? 'bottom (vector-ref range-vec sid))
            ; bottom, update the range
            (vector-set! range-vec sid rng)
        ]
        [(set? (vector-ref range-vec sid))
            ; a set, get interssection
            (vector-set! range-vec sid (set-intersect (vector-ref range-vec sid) rng))
        ]
        [else (picus:tool-error "unsupported range-vec value, got: ~a\n" (vector-ref range-vec sid))]
    )
)

; actual matching funtion of the lemma
; we are looking for expanded forms of (x-0)(x-1)=0 (also: x*(x+p1)=0), which is e.g.,:
;   - (assert (= (rem (+ (* ps1 y639) (* y639 y639)) p) zero))  --> not captured by ab0
;   - (assert (or (= zero (ff.add ps1 x701)) (= zero x701)))    --> captured by ab0
; need to consider grammar of finite field and non finite field
(define (process arg-r1cs range-vec)
    (define vs (r1cs:rcmds-vs arg-r1cs))
    (for ([obj (in-list vs)])
        (match obj

            ; ==================================
            ; ==== non finite field version ====
            ; ==================================

            ; - (assert (= (rem (+ (* ps1 y639) (* y639 y639)) p) zero))
            [(r1cs:rassert (r1cs:req
                (r1cs:rmod
                    (r1cs:radd (list-no-order
                        (r1cs:rmul (list-no-order (r1cs:rvar "ps1") (r1cs:rvar x0)))
                        (r1cs:rmul (list (r1cs:rvar x1) (r1cs:rvar x1)))
                    ))
                    _
                )
                (r1cs:rvar "zero")
             ))
                ; signal x is bounded to {0,1}, extract signal number
                (when (equal? x0 x1)
                    ; (printf "binary01 add: ~a\n" x0)
                    (override!-range range-vec x0 (list->set (list 0 1)))
                )
            ]
            ; flip#1
            [(r1cs:rassert (r1cs:req
                (r1cs:rvar "zero")
                (r1cs:rmod
                    (r1cs:radd (list-no-order
                        (r1cs:rmul (list-no-order (r1cs:rvar "ps1") (r1cs:rvar x0)))
                        (r1cs:rmul (list (r1cs:rvar x1) (r1cs:rvar x1)))
                    ))
                    _
                )
             ))
                ; signal x is bounded to {0,1}, extract signal number
                (when (equal? x0 x1)
                    ; (printf "binary01 add: ~a\n" x0)
                    (override!-range range-vec x0 (list->set (list 0 1)))
                )
            ]

            ; (assert (or (= zero (ff.add ps1 x701)) (= zero x701)))
            [(r1cs:rassert (r1cs:ror (list-no-order
                (r1cs:req
                    (r1cs:rvar "zero")
                    (r1cs:rmod
                        (r1cs:radd (list-no-order (r1cs:rvar "ps1") (r1cs:rvar x0)))
                        _
                    )
                )
                (r1cs:req
                    (r1cs:rvar "zero")
                    (r1cs:rvar x1)
                )
             )))
                ; signal x is bounded to {0,1}, extract signal number
                (when (equal? x0 x1)
                    ; (printf "binary01 add: ~a\n" x0)
                    (override!-range range-vec x0 (list->set (list 0 1)))
                )
            ]
            ; flip#1
            [(r1cs:rassert (r1cs:ror (list-no-order
                (r1cs:req
                    (r1cs:rvar "zero")
                    (r1cs:rmod
                        (r1cs:radd (list-no-order (r1cs:rvar "ps1") (r1cs:rvar x0)))
                        _
                    )
                )
                (r1cs:req
                    (r1cs:rvar x1)
                    (r1cs:rvar "zero")
                )
             )))
                ; signal x is bounded to {0,1}, extract signal number
                (when (equal? x0 x1)
                    ; (printf "binary01 add: ~a\n" x0)
                    (override!-range range-vec x0 (list->set (list 0 1)))
                )
            ]
            ; flip#2
            [(r1cs:rassert (r1cs:ror (list-no-order
                (r1cs:req
                    (r1cs:rmod
                        (r1cs:radd (list-no-order (r1cs:rvar "ps1") (r1cs:rvar x0)))
                        _
                    )
                    (r1cs:rvar "zero")
                )
                (r1cs:req
                    (r1cs:rvar "zero")
                    (r1cs:rvar x1)
                )
             )))
                ; signal x is bounded to {0,1}, extract signal number
                (when (equal? x0 x1)
                    ; (printf "binary01 add: ~a\n" x0)
                    (override!-range range-vec x0 (list->set (list 0 1)))
                )
            ]
            ; flip#3
            [(r1cs:rassert (r1cs:ror (list-no-order
                (r1cs:req
                    (r1cs:rmod
                        (r1cs:radd (list-no-order (r1cs:rvar "ps1") (r1cs:rvar x0)))
                        _
                    )
                    (r1cs:rvar "zero")
                )
                (r1cs:req
                    (r1cs:rvar x1)
                    (r1cs:rvar "zero")
                )
             )))
                ; signal x is bounded to {0,1}, extract signal number
                (when (equal? x0 x1)
                    ; (printf "binary01 add: ~a\n" x0)
                    (override!-range range-vec x0 (list->set (list 0 1)))
                )
            ]

            ; ==============================
            ; ==== finite field version ====
            ; ==============================

            ; - (assert (= (rem (+ (* ps1 y639) (* y639 y639)) p) zero))
            [(r1cs:rassert (r1cs:req
                (r1cs:radd (list-no-order
                    (r1cs:rmul (list-no-order (r1cs:rvar "ps1") (r1cs:rvar x0)))
                    (r1cs:rmul (list (r1cs:rvar x1) (r1cs:rvar x1)))
                ))
                (r1cs:rvar "zero")
             ))
                ; signal x is bounded to {0,1}, extract signal number
                (when (equal? x0 x1)
                    ; (printf "binary01 add: ~a\n" x0)
                    (override!-range range-vec x0 (list->set (list 0 1)))
                )
            ]
            ; flip#1
            [(r1cs:rassert (r1cs:req
                (r1cs:rvar "zero")
                (r1cs:radd (list-no-order
                    (r1cs:rmul (list-no-order (r1cs:rvar "ps1") (r1cs:rvar x0)))
                    (r1cs:rmul (list (r1cs:rvar x1) (r1cs:rvar x1)))
                ))
             ))
                ; signal x is bounded to {0,1}, extract signal number
                (when (equal? x0 x1)
                    ; (printf "binary01 add: ~a\n" x0)
                    (override!-range range-vec x0 (list->set (list 0 1)))
                )
            ]

            ; (assert (or (= zero (ff.add ps1 x701)) (= zero x701)))
            [(r1cs:rassert (r1cs:ror (list-no-order
                (r1cs:req
                    (r1cs:rvar "zero")
                    (r1cs:radd (list-no-order (r1cs:rvar "ps1") (r1cs:rvar x0)))
                )
                (r1cs:req
                    (r1cs:rvar "zero")
                    (r1cs:rvar x1)
                )
             )))
                ; signal x is bounded to {0,1}, extract signal number
                (when (equal? x0 x1)
                    ; (printf "binary01 add: ~a\n" x0)
                    (override!-range range-vec x0 (list->set (list 0 1)))
                )
            ]
            ; flip#1
            [(r1cs:rassert (r1cs:ror (list-no-order
                (r1cs:req
                    (r1cs:rvar "zero")
                    (r1cs:radd (list-no-order (r1cs:rvar "ps1") (r1cs:rvar x0)))
                )
                (r1cs:req
                    (r1cs:rvar x1)
                    (r1cs:rvar "zero")
                )
             )))
                ; signal x is bounded to {0,1}, extract signal number
                (when (equal? x0 x1)
                    ; (printf "binary01 add: ~a\n" x0)
                    (override!-range range-vec x0 (list->set (list 0 1)))
                )
            ]
            ; flip#2
            [(r1cs:rassert (r1cs:ror (list-no-order
                (r1cs:req
                    (r1cs:radd (list-no-order (r1cs:rvar "ps1") (r1cs:rvar x0)))
                    (r1cs:rvar "zero")
                )
                (r1cs:req
                    (r1cs:rvar "zero")
                    (r1cs:rvar x1)
                )
             )))
                ; signal x is bounded to {0,1}, extract signal number
                (when (equal? x0 x1)
                    ; (printf "binary01 add: ~a\n" x0)
                    (override!-range range-vec x0 (list->set (list 0 1)))
                )
            ]
            ; flip#3
            [(r1cs:rassert (r1cs:ror (list-no-order
                (r1cs:req
                    (r1cs:radd (list-no-order (r1cs:rvar "ps1") (r1cs:rvar x0)))
                    (r1cs:rvar "zero")
                )
                (r1cs:req
                    (r1cs:rvar x1)
                    (r1cs:rvar "zero")
                )
             )))
                ; signal x is bounded to {0,1}, extract signal number
                (when (equal? x0 x1)
                    ; (printf "binary01 add: ~a\n" x0)
                    (override!-range range-vec x0 (list->set (list 0 1)))
                )
            ]

            ; otherwise, do not rewrite
            [_ (void)]
        )
    )
)
