#lang racket
; (note) this is applied in the normalization phase
; this contains a list of simple and basic optimization steps
;   - add p related definition and replace p
;   - remove *1 in mul
;   - remove +0 in add
;   - rewrite *x as x
;   - rewrite +x as x
;   - replace x0 with 1
;   - partial evaluation: compute concrete results, e.g., 0*0 => 0
;     - (fixme) current only modify 0*...*x to 0
(require "../exit.rkt"
         (prefix-in utils: "../utils.rkt")
         (prefix-in config: "../config.rkt")
         (prefix-in r1cs: "../r1cs/r1cs-grammar.rkt"))
(provide optimize-r1cs)

(define (is-rint-zero x)
    (if (r1cs:rint? x)
        (if (= 0 (r1cs:rint-v x))
            #t
            #f
        )
        #f
    )
)

(define (is-rint-one x)
    (if (r1cs:rint? x)
        (if (= 1 (r1cs:rint-v x))
            #t
            #f
        )
        #f
    )
)

(define (contains-rint-zero l)
    (if (null? l)
        #f
        (let ([x (car l)])
            (if (r1cs:rint? x)
                (if (= 0 (r1cs:rint-v x))
                    #t
                    (contains-rint-zero (cdr l))
                )
                (contains-rint-zero (cdr l))
            )
        )
    )
)

(define (optimize-r1cs arg-r1cs)
    (match arg-r1cs

        ; command level
        [(r1cs:rcmds vs) (r1cs:rcmds (for/list ([v vs]) (optimize-r1cs v)))]

        [(r1cs:rraw v) (r1cs:rraw v)]
        [(r1cs:rlogic v) (r1cs:rlogic v)]
        ; (note) don't optimize declaration line
        [(r1cs:rdef v t) (r1cs:rdef v (optimize-r1cs t))]
        [(r1cs:rassert v) (r1cs:rassert (optimize-r1cs v))]
        [(r1cs:rcmt v) (r1cs:rcmt v)]
        [(r1cs:rsolve ) (r1cs:rsolve )]

        ; sub-command level
        [(r1cs:req lhs rhs) (r1cs:req (optimize-r1cs lhs) (optimize-r1cs rhs))]
        [(r1cs:rneq lhs rhs) (r1cs:rneq (optimize-r1cs lhs) (optimize-r1cs rhs))]

        [(r1cs:rand vs)
            (define new-vs (for/list ([v vs]) (optimize-r1cs v)))
            ; if there's only one element, extract content directly
            (if (= 1 (length new-vs))
                (car new-vs)
                (r1cs:rand (for/list ([v new-vs]) v))
            )
        ]
        [(r1cs:ror vs)
            (define new-vs (for/list ([v vs]) (optimize-r1cs v)))
            ; if there's only one element, extract content directly
            (if (= 1 (length new-vs))
                (car new-vs)
                (r1cs:ror (for/list ([v new-vs]) v))
            )
        ]

        [(r1cs:rint v) (r1cs:rint v)]
        ; (note) we assume "x0" is the first wire with prefix "x"
        [(r1cs:rvar v)
            (cond
                [(equal? "x0" v) (optimize-r1cs (r1cs:rint 1))]
                [else (r1cs:rvar v)]
            )
        ]
        [(r1cs:rtype v) (r1cs:rtype v)]

        [(r1cs:radd vs)
            ; remove 0
            (define new-vs (filter
                (lambda (x) (not (is-rint-zero x)))
                (for/list ([v vs]) (optimize-r1cs v))
            ))
            (cond
                ; no element, all values are 0 and filtered out, return base 0
                [(= 0 (length new-vs)) (r1cs:rint 0)]
                ; if there's only one element, rewrite to neg
                [(= 1 (length new-vs)) (car new-vs)]
                ; do nothing
                [else (r1cs:radd new-vs)]
            )
        ]
        [(r1cs:rmul vs)
            (define new-vs (filter
                (lambda (x) (not (is-rint-one x)))
                (for/list ([v vs]) (optimize-r1cs v))
            ))
            (cond
                ; if there's zero already in multiplication, directly return 0
                [(contains-rint-zero new-vs) (r1cs:rint 0)]
                ; no element, all values are 1 and filtered out, return base 1
                [(= 0 (length new-vs)) (r1cs:rint 1)]
                ; if there's only one element, extract content directly
                [(= 1 (length new-vs)) (car new-vs)]
                ; do nothing
                [else (r1cs:rmul new-vs)]
            )
        ]
        [_ (picus:tool-error "not supported: ~a" arg-r1cs)]))
