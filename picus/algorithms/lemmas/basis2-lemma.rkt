#lang racket
; this implements the basis-2 lemma (similar to Ecne Rule 3/5):
;   if z = 2^0 * x0 + 2^1 * x1 + ... + 2^n * xn, and x0, x1, ..., xn are all in {0,1}
;   then if z is uniquely determined, so as x0, x1, ..., xn
; this requires p1cnsts
(require
    (prefix-in config: "../../config.rkt")
    (prefix-in tokamak: "../../tokamak.rkt")
    (prefix-in r1cs: "../../r1cs/r1cs-grammar.rkt")
)
(provide (rename-out
    [apply-lemma apply-lemma]
))

; recursively apply linear lemma
(define (apply-lemma ks us p1cnsts range-vec)
    (printf "  # propagation (basis2 lemma): ")
    (define tmp-ks (list->set (set->list ks)))
    (define tmp-us (list->set (set->list us)))

    (set!-values (tmp-ks tmp-us) (process tmp-ks tmp-us p1cnsts range-vec))
    (let ([s0 (set-subtract tmp-ks ks)])
        (if (set-empty? s0)
            (printf "none.\n")
            (printf "~a added.\n" s0)
        )
    )

    ; apply once is enough, return
    (values tmp-ks tmp-us)
)

;; to be initialized later in the function update
(define basis2-seqs #f)

(define (extract-signal-id x) (string->number (substring x 1)))
(define (ffsub v) (- config:p v))

(define (all-binary01? range-vec sids)
    (if (null? sids)
        #t
        (let ([sid (car sids)][sids-rest (cdr sids)])
            (if (set? (vector-ref range-vec sid))
                ; it's a set, then chec set value
                (cond
                    [(or
                        (equal? (set 0 1) (vector-ref range-vec sid))
                        (equal? (set 0) (vector-ref range-vec sid))
                        (equal? (set 1) (vector-ref range-vec sid))
                    )
                        (all-binary01? range-vec sids-rest)
                    ]
                    [else #f]
                )
                ; not a set, then no
                #f
            )
        )
    )
)

(define (process ks us arg-r1cs range-vec)
    (define tmp-ks (list->set (set->list ks)))
    (define tmp-us (list->set (set->list us)))
    (for ([obj (r1cs:rcmds-vs arg-r1cs)])
        (match obj

            ; ==================================
            ; ==== non finite field version ====
            ; ==================================
            ; pattern: 0 = a0x0 + a1x1 + ... + anxn + x
            ; use vs here since there's also ps1/ps2/ps4 that could fall into the loop
            ; so it could be (rvar "ps1") or (rint 2^n)
            [(r1cs:rassert (r1cs:req
                (r1cs:rvar "zero")
                (r1cs:rmod
                    (r1cs:radd (list-no-order
                        (r1cs:rvar x0)
                        (r1cs:rmul (list-no-order vs (r1cs:rvar xs))) ...
                    ))
                    _
                )
             ))
                ; (fixme) vs could be matched to x?? since it's not typed in the pattern
                ;         need a procedure to adjust this
                ; (printf "matched.\n")
                ; (when (equal? x0 "x2059")
                ;     (printf "x2059 matched.\n")
                    ; (printf "vs: ~a\n" vs)
                    ; (printf "xs: ~a\n" xs)
                ; )
                (set!-values (tmp-ks tmp-us) (update tmp-ks tmp-us x0 vs xs range-vec))
            ]
            ; flip
            [(r1cs:rassert (r1cs:req
                (r1cs:rmod
                    (r1cs:radd (list-no-order
                        (r1cs:rvar x0)
                        (r1cs:rmul (list-no-order vs (r1cs:rvar xs))) ...
                    ))
                    _
                )
                (r1cs:rvar "zero")
             ))
                (set!-values (tmp-ks tmp-us) (update tmp-ks tmp-us x0 vs xs range-vec))
            ]

            ; ==============================
            ; ==== finite field version ====
            ; ==============================
            [(r1cs:rassert (r1cs:req
                (r1cs:rvar "zero")
                (r1cs:radd (list-no-order
                    (r1cs:rvar x0)
                    (r1cs:rmul (list-no-order vs (r1cs:rvar xs))) ...
                ))
             ))
                (set!-values (tmp-ks tmp-us) (update tmp-ks tmp-us x0 vs xs range-vec))
            ]
            ; flip
            [(r1cs:rassert (r1cs:req
                (r1cs:radd (list-no-order
                    (r1cs:rvar x0)
                    (r1cs:rmul (list-no-order vs (r1cs:rvar xs))) ...
                ))
                (r1cs:rvar "zero")
             ))
                (set!-values (tmp-ks tmp-us) (update tmp-ks tmp-us x0 vs xs range-vec))
            ]

            ; otherwise, do not rewrite
            [_ (void)]
        )
    )
    (values tmp-ks tmp-us)
)

(define (update ks us x0 vs xs range-vec)
    (unless basis2-seqs
      (set! basis2-seqs
            (for/set ([i (range (floor (log config:p 2)))])
              (for/set ([j (range (+ 1 i))])
                (expt 2 j)))))
    (define tmp-ks (list->set (set->list ks)))
    (define tmp-us (list->set (set->list us)))
    ; extract coefficients
    ; need to remap by calling p-v since they are all in form of pv
    ; when moved to the other side they become v
    (define coelist (for/list ([v vs])
        (match v
            [(r1cs:rvar "ps1") (- config:p 1)]
            [(r1cs:rvar "ps2") (- config:p 2)]
            [(r1cs:rvar "ps4") (- config:p 4)]
            [(r1cs:rint z) z]
            [_ (tokamak:exit "unsupported coefficient, got: ~a" v)]
        )
    ))
    (define coelist2 (for/list ([v vs])
        (match v
            [(r1cs:rvar "ps1") (ffsub (- config:p 1))]
            [(r1cs:rvar "ps2") (ffsub (- config:p 2))]
            [(r1cs:rvar "ps4") (ffsub (- config:p 4))]
            [(r1cs:rint z) (ffsub z)]
            [_ (tokamak:exit "unsupported coefficient, got: ~a" v)]
        )
    ))
    (define coeset (list->set coelist))
    (define coeset2 (list->set coelist2))
    (define siglist (for/list ([x xs]) (extract-signal-id x)))
    (cond
        [(= (length coelist) (set-count coeset))
            ; (printf "hi: ~a\n" (set-member? basis2-seqs coeset))
            ; (printf "coeset2 ~a\n" coeset2)
            ; (when (equal? x0 "x2059")
            ;     (printf "coeset2: ~a\n" coeset2)
            ;     (printf "siglist: ~a\n" siglist)
            ; )
            (if (or (set-member? basis2-seqs coeset) (set-member? basis2-seqs coeset2))
                ; yes it's a basis sequence
                ; check for signal ranges
                (if (all-binary01? range-vec siglist)
                    ; good, all binary01
                    ; check if the target signal is already unique
                    (if (set-member? ks (extract-signal-id x0))
                        ; yes it's unique, then add all basis signals to known set
                        (begin
                            (set! tmp-ks (set-union tmp-ks (list->set siglist)))
                            (set! tmp-us (set-remove tmp-us (list->set siglist)))
                            ; (when (equal? x0 "x2059")
                            ;     (printf "succeed.\n")
                            ;     (printf "old ks: ~a\n" ks)
                            ;     (printf "tmp ks: ~a\n" tmp-ks)
                            ; )
                        )
                        ; no
                        (void)
                    )
                    ; no good
                    (void)
                )
                ; not a basis sequence
                (void)
            )
        ]
        [else
            ; there are duplicate numbers, can't apply basis lemma, skip
            ; (note) duplicate variables are acceptable, but duplicate bases ar enot
            (void)
        ]
    )
    (values tmp-ks tmp-us)
)
