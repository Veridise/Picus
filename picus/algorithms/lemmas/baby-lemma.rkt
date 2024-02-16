#lang racket
; this implements the baby lemma
;   p1 * x3 * x6 = p1 * x7
;   p1 * x4 * x5 = p1 * x8
;   (a * x3 + p1 * x4) * (x5 + x6) = p1 * x9
;   p1 * x7 * x8 = p1 * x10
;   (1 + d * x10) * x1 = x7 + x8
;   (1 + pd * x10) * x2 = a * x7 + p1 * x8 + x9
;   where a=168700, d=168696, p?=p-?
;   if x3, x4, x5 and x6 are all uniquely determined
;   then x1 and x2 are uniquely determined, so as other intermediate signals

;  (fixme) currently this lemma implementation is not super efficient
(require (prefix-in r1cs: "../../r1cs/r1cs-grammar.rkt")
         "../../logging.rkt")
(provide apply-lemma)

(define (apply-lemma ks us p1cnsts)
    (picus:log-progress "[baby lemma] starting propagation")
    (define tmp-ks (list->set (set->list ks)))
    (define tmp-us (list->set (set->list us)))

    (set!-values (tmp-ks tmp-us) (process tmp-ks tmp-us p1cnsts))
    (let ([s0 (set-subtract tmp-ks ks)])
        (if (set-empty? s0)
            (picus:log-debug "[baby lemma] nothing added")
            (picus:log-debug "[baby lemma] adding ~e" s0)))

    ; apply once is enough, return
    (values tmp-ks tmp-us)
)

; bag of signals: (x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
(define (process ks us arg-r1cs)
    (define tmp-ks (list->set (set->list ks)))
    (define tmp-us (list->set (set->list us)))
    (for ([obj (r1cs:rcmds-vs arg-r1cs)])
        (define mvec (build-vector 11 (lambda (n) null)))
        (vector-set! mvec 0 0)
        (if (match-x2 obj mvec)
            ; successful, continue to match x1
            (for ([obj2 (r1cs:rcmds-vs arg-r1cs)])
                (if (match-x1 obj2 mvec)
                    ; successful, continue to match rel
                    (for ([obj3 (r1cs:rcmds-vs arg-r1cs)])
                        (if (match-rel obj3 mvec)
                            ; successful, check for uniqueness status of x7, x8 and x9
                            (begin
                                (if (and
                                        (set-member? tmp-ks (vector-ref mvec 7))
                                        (set-member? tmp-ks (vector-ref mvec 8))
                                        (set-member? tmp-ks (vector-ref mvec 9))
                                        (set-member? tmp-ks (vector-ref mvec 10))
                                    )
                                    ; yes, conclude and add x1 x2 to known set
                                    (begin
                                        (set! tmp-ks (set-union tmp-ks (list->set (list
                                            (vector-ref mvec 1)
                                            (vector-ref mvec 2)
                                        ))))
                                        (set! tmp-us (set-subtract tmp-us (list->set (list
                                            (vector-ref mvec 1)
                                            (vector-ref mvec 2)
                                        ))))
                                    )
                                    ; no, continue
                                    (void)
                                )
                            )
                            ; not successful, continue
                            (void)
                        )
                    )
                    ; not successful, continue
                    (void)
                )
            )
            ; not successful, continue
            (void)
        )
    )
    (values tmp-ks tmp-us)
)

; match x2 x7 x8 x9 x10
(define (match-x2 arg-obj arg-vec)
    (match arg-obj

        ; ==================================
        ; ==== non finite field version ====
        ; ==================================
        ; pattern: (1 + pd * x10) * x2 = a * x7 + p1 * x8 + x9
        ; (assert (=
        ;   (rem
        ;     (+ x2 (* 21888242871839275222246405745257275088548364400416034343698204186575808326921 (* x10 x2)))
        ;     p
        ;   )
        ;   (rem
        ;     (+ (* 168700 x7) (+ (* ps1 x8) x9))
        ;     p
        ;   )
        ; ))
        ; (fixme) x10 and x2b could be mixed!
        [(r1cs:rassert (r1cs:req
            (r1cs:rmod
                (r1cs:radd (list-no-order
                    (r1cs:rvar x2a)
                    (r1cs:rmul (list-no-order
                        (r1cs:rint 21888242871839275222246405745257275088548364400416034343698204186575808326921)
                        (r1cs:rvar x10)
                        (r1cs:rvar x2b)
                    ))
                ))
                _
            )
            (r1cs:rmod
                (r1cs:radd (list-no-order
                    (r1cs:rmul (list-no-order
                        (r1cs:rint 168700)
                        (r1cs:rvar x7)
                    ))
                    (r1cs:rmul (list-no-order
                        (r1cs:rvar "ps1")
                        (r1cs:rvar x8)
                    ))
                    (r1cs:rvar x9)
                ))
                _
            )
         ))
            (if (equal? x2a x2b)
                (begin
                    (vector-set! arg-vec 2 x2a)
                    (vector-set! arg-vec 7 x7)
                    (vector-set! arg-vec 8 x8)
                    (vector-set! arg-vec 9 x9)
                    (vector-set! arg-vec 10 x10)
                    #t
                )
                #f
            )
        ]

        ; ==============================
        ; ==== finite field version ====
        ; ==============================
        [(r1cs:rassert (r1cs:req
            (r1cs:radd (list-no-order
                (r1cs:rvar x2a)
                (r1cs:rmul (list-no-order
                    (r1cs:rint 21888242871839275222246405745257275088548364400416034343698204186575808326921)
                    (r1cs:rvar x10)
                    (r1cs:rvar x2b)
                ))
            ))
            (r1cs:radd (list-no-order
                (r1cs:rmul (list-no-order
                    (r1cs:rint 168700)
                    (r1cs:rvar x7)
                ))
                (r1cs:rmul (list-no-order
                    (r1cs:rvar "ps1")
                    (r1cs:rvar x8)
                ))
                (r1cs:rvar x9)
            ))
         ))
            (if (equal? x2a x2b)
                (begin
                    (vector-set! arg-vec 2 x2a)
                    (vector-set! arg-vec 7 x7)
                    (vector-set! arg-vec 8 x8)
                    (vector-set! arg-vec 9 x9)
                    (vector-set! arg-vec 10 x10)
                    #t
                )
                #f
            )
        ]

        ; otherwise, do not rewrite
        [_ #f]
    )
)

; returns (list x1)
(define (match-x1 arg-obj arg-vec)
    (match arg-obj

        ; ==================================
        ; ==== non finite field version ====
        ; ==================================
        ; pattern: (1 + d * x10) * x1 = x7 + x8
        ; (assert (= (rem (+ x1 (* 168696 (* x10 x1))) p) (rem (+ x7 x8) p)))
        [(r1cs:rassert (r1cs:req
            (r1cs:rmod
                (r1cs:radd (list-no-order
                    (r1cs:rvar x1a)
                    (r1cs:rmul (list-no-order
                        (r1cs:rint 168696)
                        (r1cs:rvar x10)
                        (r1cs:rvar x1b)
                    ))
                ))
                _
            )
            (r1cs:rmod
                (r1cs:radd (list-no-order
                    (r1cs:rvar x7)
                    (r1cs:rvar x8)
                ))
                _
            )
         ))
            (if (and
                    (equal? x1a x1b)
                    (equal? (vector-ref arg-vec 7) x7)
                    (equal? (vector-ref arg-vec 8) x8)
                    (equal? (vector-ref arg-vec 10) x10)
                )
                (begin
                    (vector-set! arg-vec 1 x1a)
                    #t
                )
                #f
            )
        ]

        ; ==============================
        ; ==== finite field version ====
        ; ==============================
        [(r1cs:rassert (r1cs:req
            (r1cs:radd (list-no-order
                (r1cs:rvar x1a)
                (r1cs:rmul (list-no-order
                    (r1cs:rint 168696)
                    (r1cs:rvar x10)
                    (r1cs:rvar x1b)
                ))
            ))
            (r1cs:radd (list-no-order
                (r1cs:rvar x7)
                (r1cs:rvar x8)
            ))
         ))
            (if (and
                    (equal? x1a x1b)
                    (equal? (vector-ref arg-vec 7) x7)
                    (equal? (vector-ref arg-vec 8) x8)
                    (equal? (vector-ref arg-vec 10) x10)
                )
                (begin
                    (vector-set! arg-vec 1 x1a)
                    #t
                )
                #f
            )
        ]

        ; otherwise, do not rewrite
        [_ #f]
    )
)

(define (match-rel arg-obj arg-vec)
    (match arg-obj

        ; ==================================
        ; ==== non finite field version ====
        ; ==================================
        ; pattern: p1 * x7 * x8 = p1 * x10
        ; (assert (= (rem (* ps1 (* x7 x8)) p) (rem (* ps1 x10) p)))
        [(r1cs:rassert (r1cs:req
            (r1cs:rmod
                (r1cs:rmul (list-no-order
                    (r1cs:rvar "ps1")
                    (r1cs:rvar x7)
                    (r1cs:rvar x8)
                ))
                _
            )
            (r1cs:rmod
                (r1cs:rmul (list-no-order
                    (r1cs:rvar "ps1")
                    (r1cs:rvar x10)
                ))
                _
            )
         ))
            (if (and
                    (equal? (vector-ref arg-vec 7) x7)
                    (equal? (vector-ref arg-vec 8) x8)
                    (equal? (vector-ref arg-vec 10) x10)
                )
                #t
                #f
            )
        ]

        ; ==============================
        ; ==== finite field version ====
        ; ==============================
        [(r1cs:rassert (r1cs:req
            (r1cs:rmul (list-no-order
                (r1cs:rvar "ps1")
                (r1cs:rvar x7)
                (r1cs:rvar x8)
            ))
            (r1cs:rmul (list-no-order
                (r1cs:rvar "ps1")
                (r1cs:rvar x10)
            ))
         ))
            (if (and
                    (equal? (vector-ref arg-vec 7) x7)
                    (equal? (vector-ref arg-vec 8) x8)
                    (equal? (vector-ref arg-vec 10) x10)
                )
                #t
                #f
            )
        ]

        ; otherwise, do not rewrite
        [_ #f]
    )
)
