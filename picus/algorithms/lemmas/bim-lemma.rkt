#lang racket
; this implements the big-int-mul lemma
; Ax - b = 0
; if A is constant squared matrix, det(A) != 0, and b is unique
; then x is unique
; (fixme) one-take implementation, you need a big fix on this
; (fixme) this impelements a special case where b = 0
(require math
         (prefix-in config: "../../config.rkt")
         (prefix-in tokamak: "../../tokamak.rkt")
         (prefix-in r1cs: "../../r1cs/r1cs-grammar.rkt")
         "../../verbose.rkt")
(provide apply-lemma)

; recursively apply linear lemma
(define (apply-lemma ks us p1cnsts range-vec)
    (vprintf "  propagation (bim lemma): ")
    (define-values (tmp-ks tmp-us) (process ks us p1cnsts range-vec))
    (let ([s0 (set-subtract tmp-ks ks)])
        (if (set-empty? s0)
            (vprintf "none.\n")
            (vprintf "~e added.\n" s0)))

    ; apply once is enough, return
    (values tmp-ks tmp-us)
)

(define (extract-signal-id x) (string->number (substring x 1)))
(define (signal? x) (or (string-prefix? x "x") (string-prefix? x "y")))
(define (extract-constant x)
    (cond
        [(equal? x "p") config:p]
        [(equal? x "ps1") (- config:p 1)]
        [(equal? x "ps2") (- config:p 2)]
        [(equal? x "ps3") (- config:p 3)]
        [(equal? x "ps4") (- config:p 4)]
        [(equal? x "ps5") (- config:p 5)]
        [(equal? x "zero") 0]
        [(equal? x "one") 1]
        [else (tokamak:exit "unsupported constant, got: ~a" x)]
    )
)

(define (process ks us arg-r1cs range-vec)
    (define tmp-ks (list->set (set->list ks)))
    (define tmp-us (list->set (set->list us)))
    (define eqs (list ))
    (for ([v (r1cs:rcmds-vs arg-r1cs)])
        (define res (match-single v))
        (when (list? res)
            (set! eqs (cons res eqs))
        )
    )
    ; (printf "# [debug] eqs is: ~a\n" (reverse eqs))
    ; sort and construct matrix
    (define eqmaps (make-vector (length eqs) null))
    (define smap (make-hash))
    (for ([eq (in-list eqs)] [i (in-naturals)])
        (vector-set! eqmaps i (make-hash eq))
        (for ([p eq])
            (when (not (hash-has-key? smap (car p)))
                (hash-set! smap (car p) (hash-count smap)))
        )
    )
    (define slist (hash-values smap))
    (define nrow (vector-length eqmaps))
    (define ncol (hash-count smap))
    ; (printf "# [debug] row: ~a, col: ~a\n" (vector-length eqmaps) (hash-count smap))
    ; (printf "# [debug] smaps is ~a\n" smaps)
    (cond
        [(and (equal? nrow ncol) (> nrow 0))
            ; construct A matrix
            (define Amtx (build-matrix nrow ncol (lambda (r c)
                (let ([em0 (vector-ref eqmaps r)][sig0 (list-ref slist c)])
                    (if (hash-has-key? em0 sig0) (hash-ref em0 sig0) 0)
                )
            )))
            (define det (matrix-determinant Amtx))
            (when (not (equal? 0 det))
                (set! tmp-ks (set-union tmp-ks (list->set slist)))
                (set! tmp-us (set-subtract tmp-us (list->set slist)))
            )
        ]
        ; else do nothing since it's not a squared matrix
    )
    (values tmp-ks tmp-us)
)

; match 0 = a * x0 + ... + c * xn form
; extract list of pairs of (coefficient signal-id)
(define (match-single arg-obj)
    (match arg-obj

        ; ==================================
        ; ==== non finite field version ====
        ; ==================================
        [(r1cs:rassert (r1cs:req
            (r1cs:rvar "zero")
            (r1cs:rmod (r1cs:radd (list vs ...)) (r1cs:rvar "p"))
        ))
            ; top level match, go to match smaller ones
            (define res (for/list ([v vs]) (extract-pairs v)))
            (if (set-member? (list->set res) #f)
                #f
                res
            )
        ]

        ; ==================================
        ; ==== finite field version ====
        ; ==================================
        [(r1cs:rassert (r1cs:req
            (r1cs:rvar "zero")
            (r1cs:radd (list vs ...))
        ))
            ; top level match, go to match smaller ones
            (define res (for/list ([v vs]) (extract-pairs v)))
            (if (set-member? (list->set res) #f)
                #f
                res
            )
        ]

        ; otherwise, do not rewrite
        [_ #f]
    )
)

; pair is (signal-id coefficient)
(define (extract-pairs arg-obj)
    (match arg-obj
        ; ==================================
        ; ==== non finite field version ====
        ; ==================================
        [(r1cs:rvar x)
            (if (signal? x)
                ; coefficient is one
                (cons (extract-signal-id x) 1)
                ; not a signal, this is probably a named constant, abort
                #f
            )

        ]
        [(r1cs:rmul (list (r1cs:rvar c) (r1cs:rvar x)))
            (if (and (not (signal? c)) (signal? x))
                ; correct format
                (cons (extract-signal-id x) (extract-constant c))
                ; incorrect format, give up
                #f
            )
        ]
        [_ #f]
    )
)
