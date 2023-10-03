#lang racket
; (note) this is applied in optimization phase 0
; this implements the following lemma
;   A * B = 0 => A = 0 or B = 0
(require
    (prefix-in config: "../config.rkt")
    (prefix-in r1cs: "../r1cs/r1cs-grammar.rkt")
)
(provide (rename-out
    [optimize-r1cs optimize-r1cs]
))

(define (optimize-r1cs arg-r1cs)
    (match arg-r1cs

        ; command level
        [(r1cs:rcmds vs) (r1cs:rcmds (for/list ([v vs]) (optimize-r1cs v)))]
        [(r1cs:rassert v) (r1cs:rassert (optimize-r1cs v))]

        ; (note) yes, this is the specific shape in standard form about A*B=0
        [(r1cs:req
            (r1cs:rmod (r1cs:rmul (list vs ...)) _)
            (r1cs:rmod (r1cs:radd (list (r1cs:rint 0))) _)
         )
            ; rewrite
            (r1cs:ror
                (for/list ([v vs])
                    (r1cs:req
                        (r1cs:rint 0)
                        (r1cs:rmod v (r1cs:rint config:p))
                    )
                )
            )
        ]

        [(r1cs:req
            (r1cs:rmod (r1cs:radd (list (r1cs:rint 0))) _)
            (r1cs:rmod (r1cs:rmul (list vs ...)) _)
         )
            ; rewrite
            (r1cs:ror
                (for/list ([v vs])
                    (r1cs:req
                        (r1cs:rint 0)
                        (r1cs:rmod v (r1cs:rint config:p))
                    )
                )
            )
        ]

        ; otherwise, do not rewrite
        [_ arg-r1cs]
    )
)
