#lang racket
(require "../exit.rkt"
         (prefix-in utils: "../utils.rkt")
         (prefix-in config: "../config.rkt"))
(provide (all-defined-out))

; reference: https://github.com/franklynwang/EcneProject/blob/master/src/R1CSConstraintSolver.jl#L10
; reference: https://github.com/franklynwang/EcneProject/blob/master/src/R1CSConstraintSolver.jl#L13
; map back to field
(define (F x) (modulo x config:p))

; ====================================
; ======== utils for r1cs ast ========
; ====================================

; r1cs commands (grammar)
; command level
(struct rcmds (vs) #:mutable #:transparent #:reflection-name 'r1cs:rcmds) ; vs: list
(struct rraw (v) #:mutable #:transparent #:reflection-name 'r1cs:rraw) ; any raw command, v: str
(struct rlogic (v) #:mutable #:transparent #:reflection-name 'r1cs:rlogic) ; v: logic
(struct rdef (var type) #:mutable #:transparent #:reflection-name 'r1cs:rdef) ; var: rvar, type: rtype
(struct rassert (cnst) #:mutable #:transparent #:reflection-name 'r1cs:rassert) ; cnst: req | rneq | rleq | ...
(struct rcmt (v) #:mutable #:transparent #:reflection-name 'r1cs:rcmt) ; comments
(struct rsolve () #:mutable #:transparent #:reflection-name 'r1cs:rsolve)
; sub-command level
(struct rint (v) #:mutable #:transparent #:reflection-name 'r1cs:rint) ; v: int
(struct rvar (v) #:mutable #:transparent #:reflection-name 'r1cs:rvar) ; v: str
(struct rtype (v) #:mutable #:transparent #:reflection-name 'r1cs:rtype) ; v: str
; sub-command level
(struct req (lhs rhs) #:mutable #:transparent #:reflection-name 'r1cs:req)
(struct rneq (lhs rhs) #:mutable #:transparent #:reflection-name 'r1cs:rneq)
(struct rleq (lhs rhs) #:mutable #:transparent #:reflection-name 'r1cs:rleq)
(struct rlt (lhs rhs) #:mutable #:transparent #:reflection-name 'r1cs:rlt)
(struct rgeq (lhs rhs) #:mutable #:transparent #:reflection-name 'r1cs:rgeq)
(struct rgt (lhs rhs) #:mutable #:transparent #:reflection-name 'r1cs:rgt)
; sub-command level
(struct rand (vs) #:mutable #:transparent #:reflection-name 'r1cs:rand) ; vs: a list of bools
(struct ror (vs) #:mutable #:transparent #:reflection-name 'r1cs:ror) ; vs: a list of bools
(struct rimp (lhs rhs) #:mutable #:transparent #:reflection-name 'r1cs:imp)

; no rnot here out of simplicity, use neq, or other unequalty instead
; sub-command level
(struct radd (vs) #:mutable #:transparent #:reflection-name 'r1cs:radd) ; vs: list
(struct rmul (vs) #:mutable #:transparent #:reflection-name 'r1cs:rmul) ; vs: list
(struct rmod (v mod) #:mutable #:transparent #:reflection-name 'r1cs:rmod) ; v: int, mod: int

(define (rcmds-ref obj ind) (list-ref (rcmds-vs obj) ind))

; concatenate multiple rcmd
(define (rcmds-append . obj)
    (if (null? obj)
        (rcmds (list ))
        (rcmds (append
            (rcmds-vs (car obj))
            (rcmds-vs (apply rcmds-append (cdr obj)))
        ))
    )
)

; returns a human readable string of one specified equation
; no mode is shown for better reading
; this will skip the solver specific commands
(define (rcmds->string arg-rcmds arg-id)
    ; define print function
    (define (do obj0)
        (match obj0
            [(rraw v) ""]
            [(rlogic v) ""]
            [(rdef v t) ""]
            [(rassert v) (do v)]
            [(rcmt v) ""]
            [(rsolve ) ""]
            [(req lhs rhs) (format "~a = ~a" (do lhs) (do rhs))]
            [(rneq lhs rhs) (format "~a != ~a" (do lhs) (do rhs))]
            [(rleq lhs rhs) (format "~a <= ~a" (do lhs) (do rhs))]
            [(rlt lhs rhs) (format "~a < ~a" (do lhs) (do rhs))]
            [(rgeq lhs rhs) (format "~a >= ~a" (do lhs) (do rhs))]
            [(rgt lhs rhs) (format "~a > ~a" (do lhs) (do rhs))]
            [(rand vs)
                (string-join (for/list ([v vs]) (format "(~a)" (do v)))
                    " /\\ "
                )
            ]
            [(rimp lhs rhs) (format "~a => ~a" (do lhs) (do rhs))]
            [(ror vs)
                (string-join (for/list ([v vs]) (format "(~a)" (do v)))
                    " \\/ "
                )
            ]
            [(rint v) (format "~a" v)]
            [(rvar v) (format "~a" v)]
            [(radd vs)
                (string-join (for/list ([v vs]) (format "~a" (do v)))
                    " + "
                )
            ]
            [(rmul vs)
                (string-join (for/list ([v vs]) (format "~a" (do v)))
                    " * "
                )
            ]
            [(rmod v mod) (do v)]
            [_ (picus:tool-error "not supported: ~a" obj0)]
        )
    )
    ; call print function and return the printed string
    (do (list-ref (rcmds-vs arg-rcmds) arg-id))
)

; return a set of all variables occuring in a (partial) r1cs ast
;   - arg-indexonly: whether to extract the indices instead of keeping the full variable name
; (note) you should (had better) normalize the constraints before calling this method
;        to get the most precise results; e.g., 1*x would not otherwise be considered
;        in form of x0*x
(define (get-assert-variables obj [arg-indexonly #f])
    ; define internal function
    (define (do obj0)
        (match obj0
            [(rcmds vs) (append* (for/list ([v vs]) (do v)))]
            [(rraw v) (list )]
            [(rlogic v) (list )]
            [(rdef v t) (list )]
            [(rassert v) (do v)]
            [(rcmt v) (list )]
            [(rsolve ) (list )]
            [(req lhs rhs) (append (do lhs) (do rhs))]
            [(rneq lhs rhs) (append (do lhs) (do rhs))]
            [(rleq lhs rhs) (append (do lhs) (do rhs))]
            [(rlt lhs rhs) (append (do lhs) (do rhs))]
            [(rgeq lhs rhs) (append (do lhs) (do rhs))]
            [(rgt lhs rhs) (append (do lhs) (do rhs))]
            [(rand vs) (append* (for/list ([v vs]) (do v)))]
            [(rimp lhs rhs) (append (do lhs) (do rhs))]
            [(ror vs) (append* (for/list ([v vs]) (do v)))]
            [(rint v) (list )]
            [(rvar v)
                ; (fixme) we here assume prefix is either "x" or "y"
                (if (or (string-prefix? v "x") (string-prefix? v "y"))
                    ; starts with x or y, good
                    (if arg-indexonly
                        ; extracted variable index
                        (list (string->number (substring v 1)))
                        ; original string variable
                        (list v)
                    )
                    ; not starting with x or y, could be ps?, could be zero/one
                    ; don't include
                    (list )
                )
            ]
            [(rtype v) (list )]
            [(radd vs) (append* (for/list ([v vs]) (do v)))]
            [(rmul vs) (append* (for/list ([v vs]) (do v)))]
            [(rmod v mod) (append (do v) (do mod))]
            [_ (picus:tool-error "not supported: ~a" obj0)]
        )
    )
    ; then call it and remove duplicates, and return
    (list->set (do obj))
)

; same as get-assert-variables, but limited to linear ones
; linear meaning: variable that do not multiply with another variable (even itself)
;                 a linear variable exists to a single constraints, not a constraint system
;                 i.e., it could be linear in one constraint, while not in another
; (note) you should normalize the constraints before calling this method
;        to get the most precise results; e.g., 1*x would not otherwise be considered
;        in form of x0*x
; (important) one variable can simultaneously appear as linear and non-linear
;             in the interest of uniqueness, it should be marked as non-linear
;             but this method does not make the decision for the user
;             you need to determine by yourself
(define (get-assert-variables/linear obj [arg-indexonly #f])
    ; define internal function
    (define (do obj0)
        (match obj0
            [(rcmds vs) (append* (for/list ([v vs]) (do v)))]
            [(rraw v) (list )]
            [(rlogic v) (list )]
            [(rdef v t) (list )]
            [(rassert v) (do v)]
            [(rcmt v) (list )]
            [(rsolve ) (list )]
            [(req lhs rhs) (append (do lhs) (do rhs))]
            [(rneq lhs rhs) (append (do lhs) (do rhs))]
            [(rleq lhs rhs) (append (do lhs) (do rhs))]
            [(rlt lhs rhs) (append (do lhs) (do rhs))]
            [(rgeq lhs rhs) (append (do lhs) (do rhs))]
            [(rgt lhs rhs) (append (do lhs) (do rhs))]
            [(rand vs) (picus:tool-error "get-assert-variables/linear receives rand, which is not supported")]
            [(rimp lhs rhs) (append (do lhs) (do rhs))]
            [(ror vs) (picus:tool-error "get-assert-variables/linear receives ror, which is not supported")]
            [(rint v) (list )]
            [(rvar v)
                (if arg-indexonly
                    ; extracted variable index
                    ; (fixme) we here assume prefix is either "x" or "y"
                    (list (string->number (substring v 1)))
                    ; original string variable
                    (list v)
                )
            ]
            [(rtype v) (list )]
            [(radd vs) (append* (for/list ([v vs]) (do v)))]
            ; (assumed optimized version)
            ; - 1*x will be x, which will not be captured by rmul
            ; - (fixme) this could be wrong, a*x is treated as linear,
            ;           since prime field has unique multiplicative inverse, so a*x=b
            ;           will have unique solution for x
            ; others are not linear
            [(rmul vs)
                (let ([vv (filter (lambda (x) (rvar? x)) vs)])
                    (if (= 1 (length vv))
                        ; only 1 var, good as linear
                        (do (car vv))
                        ; no var or more than 1 var, not good for linear
                        (list )
                    )
                )
            ]
            [(rmod v mod) (append (do v) (do mod))]
            [_ (picus:tool-error "not supported: ~a" obj0)]
        )
    )
    ; then call it and remove duplicates, and return
    (list->set (do obj))
)

(define (get-assert-variables/nonlinear obj [arg-indexonly #f])
    ; define internal function
    (define (do obj0 [include? #f])
        (match obj0
            [(rcmds vs) (append* (for/list ([v vs]) (do v include?)))]
            [(rraw v) (list )]
            [(rlogic v) (list )]
            [(rdef v t) (list )]
            [(rassert v) (do v include?)]
            [(rcmt v) (list )]
            [(rsolve ) (list )]
            [(req lhs rhs) (append (do lhs include?) (do rhs include?))]
            [(rneq lhs rhs) (append (do lhs include?) (do rhs include?))]
            [(rleq lhs rhs) (append (do lhs include?) (do rhs include?))]
            [(rlt lhs rhs) (append (do lhs include?) (do rhs include?))]
            [(rgeq lhs rhs) (append (do lhs include?) (do rhs include?))]
            [(rgt lhs rhs) (append (do lhs include?) (do rhs include?))]
            [(rand vs) (picus:tool-error "get-assert-variables/nonlinear receives rand, which is not supported")]
            [(rimp lhs rhs) (append (do lhs) (do rhs))]
            [(ror vs) (picus:tool-error "get-assert-variables/nonlinear receives ror, which is not supported")]
            [(rint v) (list )]
            [(rvar v)
                ; only include when include? is #t
                (if include?
                    (if arg-indexonly
                        ; extracted variable index
                        ; (fixme) we here assume prefix is either "x" or "y"
                        (list (string->number (substring v 1)))
                        ; original string variable
                        (list v)
                    )
                    (list)
                )
            ]
            [(rtype v) (list )]
            [(radd vs)
             (append* (for/list ([v vs]) (do v include?)))]
            [(rmul vs)
             (define vv (filter (lambda (x) (rvar? x)) vs))
             (if (<= (length vv) 1)
                 ; less than or equal to 1 var, this is just linear
                 '()
                 ; more than 1 var, all involved vars are non-linear
                 (append* (for/list ([v vs]) (do v #t))))]
            [(rmod v mod) (append (do v include?) (do mod include?))]
            [_ (picus:tool-error "not supported: ~a" obj0)]
        )
    )
    ; then call it and remove duplicates, and return
    (list->set (do obj))
)

; =======================================
; ======== utils for binary r1cs ========
; =======================================

; r1cs structs for binary files (*.r1cs)
(struct r1cs (magic version nsec header constraint w2l inputs outputs) #:mutable #:transparent #:reflection-name 'r1cs)
    (struct header-section (field-size prime-number nwires npubout npubin nprvin nlabels mconstraints) #:mutable #:transparent #:reflection-name 'header-section)
    (struct constraint-section (constraints) #:mutable #:transparent #:reflection-name 'constraint-section)
        (struct constraint (a b c) #:mutable #:transparent #:reflection-name 'constraint) ; a, b and c are constraint-blocks
            (struct constraint-block (nnz wids factors) #:mutable #:transparent #:reflection-name 'constraint-block)
    (struct w2l-section (v) #:mutable #:transparent #:reflection-name 'w2l-section)

; quick functions
(define (get-mconstraints arg-r1cs) (header-section-mconstraints (r1cs-header arg-r1cs)))
(define (get-nwires arg-r1cs) (header-section-nwires (r1cs-header arg-r1cs)))
(define (get-npubout arg-r1cs) (header-section-npubout (r1cs-header arg-r1cs)))
(define (get-npubin arg-r1cs) (header-section-npubin (r1cs-header arg-r1cs)))
(define (get-nprvin arg-r1cs) (header-section-nprvin (r1cs-header arg-r1cs)))
(define (get-nlabels arg-r1cs) (header-section-nlabels (r1cs-header arg-r1cs)))
(define (get-constraints arg-r1cs) (constraint-section-constraints (r1cs-constraint arg-r1cs)))
(define (get-field-size arg-r1cs) (header-section-field-size (r1cs-header arg-r1cs)))
(define (get-prime-number arg-r1cs) (header-section-prime-number (r1cs-header arg-r1cs)))

(define (extract-header-section arg-raw)
    (define field-size (utils:bytes->number (subbytes arg-raw 0 4))) ; field size in bytes
    (when (not (zero? (remainder field-size 8)))
      (picus:tool-error "[extract-header-section] field size should be a multiple of 8, got: ~a" field-size))
    (define raw-prime (subbytes arg-raw 4 (+ 4 field-size))) ; prime number
    ;; reverse since it's big-endian
    (define bs (reverse (bytes->list raw-prime)))
    (define prime-number
      (for/fold ([val-so-far 0]) ([b (in-list bs)])
        (+ (* val-so-far 256) b)))
    (define nwires (utils:bytes->number (subbytes arg-raw (+ 4 field-size) (+ 8 field-size))))
    (define npubout (utils:bytes->number (subbytes arg-raw (+ 8 field-size) (+ 12 field-size))))
    (define npubin (utils:bytes->number (subbytes arg-raw (+ 12 field-size) (+ 16 field-size))))
    (define nprvin (utils:bytes->number (subbytes arg-raw (+ 16 field-size) (+ 20 field-size))))
    (define nlabels (utils:bytes->number (subbytes arg-raw (+ 20 field-size) (+ 28 field-size))))
    (define mconstraints (utils:bytes->number (subbytes arg-raw (+ 28 field-size) (+ 32 field-size))))
    ; return
    (header-section field-size prime-number nwires npubout npubin nprvin nlabels mconstraints)
)

(define (extract-single-constraint arg-raw arg-fs internal-pos)
    (define (extract-constraint-block arg-block arg-n)
        (define tmp-wids
            (for/list ([i arg-n])
                (define s0 (* i (+ 4 arg-fs)))
                (utils:bytes->number (subbytes arg-block s0 (+ 4 s0)))
            )
        )
        (define tmp-factors
            (for/list ([i arg-n])
                (define s0 (+ 4 (* i (+ 4 arg-fs))))
                (F (utils:bytes->number (subbytes arg-block s0 (+ arg-fs s0))))
            )
        )
        ; return
        (values tmp-wids tmp-factors)
    )

  (define (subbytes-relative start end)
    (subbytes arg-raw (+ internal-pos start) (+ internal-pos end)))

    ; block A
    (define nnz-a (utils:bytes->number (subbytes-relative 0 4))) ; number of non-zero factors
    (define block-a-start 4)
    (define block-a-end (+ block-a-start (* nnz-a (+ 4 arg-fs))))
    (define block-a (subbytes-relative block-a-start block-a-end)) ; fetch a whole block
    (define-values (wids-a factors-a) (extract-constraint-block block-a nnz-a))

    ; block B
    (define nnz-b (utils:bytes->number (subbytes-relative block-a-end (+ 4 block-a-end))))
    (define block-b-start (+ 4 block-a-end))
    (define block-b-end (+ block-b-start (* nnz-b (+ 4 arg-fs))))
    (define block-b (subbytes-relative block-b-start block-b-end))
    (define-values (wids-b factors-b) (extract-constraint-block block-b nnz-b))

    ; block C
    (define nnz-c (utils:bytes->number (subbytes-relative block-b-end (+ 4 block-b-end))))
    (define block-c-start (+ 4 block-b-end))
    (define block-c-end (+ block-c-start (* nnz-c (+ 4 arg-fs))))
    (define block-c (subbytes-relative block-c-start block-c-end))
    (define-values (wids-c factors-c) (extract-constraint-block block-c nnz-c))

    (define ret0
        (constraint
            (constraint-block nnz-a wids-a factors-a)
            (constraint-block nnz-b wids-b factors-b)
            (constraint-block nnz-c wids-c factors-c)
        )
    )
    ; return
    (values block-c-end ret0)
)

(define (extract-constraint-section arg-raw arg-fs arg-m)
  (define blen (bytes-length arg-raw))
  (define (do-extract internal-pos)
    (cond
      [(>= internal-pos blen) (list)]
      [else
       (define-values (block-end cs)
         (extract-single-constraint arg-raw arg-fs internal-pos))
       (cons cs (do-extract (+ internal-pos block-end)))]))
  (define clist (do-extract 0))
  (when (not (equal? arg-m (length clist)))
    (picus:tool-error "[extract-constraint-section] number of constraints is not equal to mconstraints, got: ~a and ~a" (length clist) arg-m))
  (constraint-section clist))

(define (extract-w2l-section arg-raw)
    ; every label id takes 8 bytes
    (when (not (zero? (remainder (bytes-length arg-raw) 8)))
      (picus:tool-error "[extract-w2l-section] bytes length should be a multiple of 8, got: ~a" (bytes-length arg-raw)))
    (define n (/ (bytes-length arg-raw) 8))
    (define map0 
        (for/list ([i n])
            (define s0 (* i 8))
            (utils:bytes->number (subbytes arg-raw s0 (+ 8 s0)))
        )
    )
    ; return
    (w2l-section map0)
)

; count the total number of sections
; arg-raw: bytes of sections without meta zone
(define (count-sections arg-raw)
    (cond 
        [(zero? (bytes-length arg-raw)) 0]
        [else
            (define section0-type (utils:bytes->number (subbytes arg-raw 0 4)))
            (define section0-size (utils:bytes->number (subbytes arg-raw 4 12)))
            (define bs0 (+ 12 section0-size)) ; next section start position
            (+ 1 (count-sections (subbytes arg-raw bs0)))
        ]
    )
)

(define (extract-section-types arg-raw)
    (cond 
        [(zero? (bytes-length arg-raw)) (list)]
        [else
            (define section0-type (utils:bytes->number (subbytes arg-raw 0 4)))
            (define section0-size (utils:bytes->number (subbytes arg-raw 4 12)))
            (define bs0 (+ 12 section0-size)) ; next section start position
            (cons section0-type (extract-section-types (subbytes arg-raw bs0)))
        ]
    )
)

; remove sections with unrecognized section type
; currently the spec only mentions types of 1, 2 and 3
(define (filter-sections arg-raw)
    (define accepted-types (list 1 2 3))
    (cond
        [(zero? (bytes-length arg-raw)) (bytes)]
        [else
            (define section0-type (utils:bytes->number (subbytes arg-raw 0 4)))
            (define section0-size (utils:bytes->number (subbytes arg-raw 4 12)))
            (define bs0 (+ 12 section0-size)) ; next section start position
            (if (utils:contains? accepted-types section0-type)
                ; yes, include this section
                (bytes-append (subbytes arg-raw 0 bs0) (filter-sections (subbytes arg-raw bs0)))
                ; no, drop this section
                (filter-sections (subbytes arg-raw bs0))
            )
        ]
    )
)

; find section start pos and size of designated section type
; arg-raw: bytes of sections without meta zone
(define (find-section arg-raw arg-type)
  (define blen (bytes-length arg-raw))
  (let loop ([pos 0])
    (cond
      [(>= pos blen)
       (picus:tool-error "[find-section-pos] cannot find position of section given type: ~a."
                         arg-type)]
      [else
       (define section0-type (utils:bytes->number (subbytes arg-raw pos (+ pos 4))))
       (define section0-size (utils:bytes->number (subbytes arg-raw (+ pos 4) (+ pos 12))))
       (cond
         [(equal? arg-type section0-type) (values pos section0-size)] ; found
         [else (loop (+ pos 12 section0-size))])])))

; format spec: https://github.com/iden3/r1csfile/blob/master/doc/r1cs_bin_format.md
(define (read-r1cs arg-path)
    (define raw (file->bytes arg-path))

    (define magic-number (subbytes raw 0 4)) ; should be #x72 #x31 #x63 #x72
    (when (not (equal? magic-number (bytes #x72 #x31 #x63 #x73)))
      (picus:tool-error "[read-r1cs] magic number is incorrect, got: ~a" magic-number))

    (define version (utils:bytes->number (subbytes raw 4 8)))
    (when (not (equal? 1 version ))
      (picus:tool-error "[read-r1cs] version is not supported, got: ~a" version))

    (define nsec (utils:bytes->number (subbytes raw 8 12)))

    (define raw-sections (subbytes raw 12)) ; 12=4+4+4, remove the meta zone
    (define fraw-sections (filter-sections raw-sections)) ; remove sections with undefined section types
    (when (not (equal? 3 (count-sections fraw-sections)))
      (picus:tool-error "[read-r1cs] r1cs needs to contain 3 sections, got: ~a" (count-sections fraw-sections)))


    ; find aand process header section, +12 to skip section meta zone
    (define-values (hs-pos hs-size) (find-section fraw-sections 1))
    (define hs0 (extract-header-section (subbytes fraw-sections (+ 12 hs-pos) (+ 12 hs-pos hs-size))))
    (define fs0 (header-section-field-size hs0))
    (define m0 (header-section-mconstraints hs0))

    ; find and process constraint section, +12 to skip section meta zone
    (define-values (cs-pos cs-size) (find-section fraw-sections 2))
    (define cs0 (extract-constraint-section 
        (subbytes fraw-sections (+ 12 cs-pos) (+ 12 cs-pos cs-size))
        fs0 m0
    ))

    ; find and process w2l section, +12 to skip section meta zone
    (define-values (ws-pos ws-size) (find-section fraw-sections 3))
    (define ws0 (extract-w2l-section (subbytes fraw-sections (+ 12 ws-pos) (+ 12 ws-pos ws-size))))

    ; compute the list of input variables (aka `knowns` in Ecne) and output variables (aka `outputs` in Ecne)
    (define istart (+ 2 (header-section-npubout hs0))) ; inclusive
    (define iend (+ 1 (header-section-npubout hs0) (header-section-npubin hs0) (header-section-nprvin hs0))) ; inclusive
    (define input-list-ecne (cons 1 
        (for/list ([i (range istart (+ 1 iend))]) i)
    ))
    (define input-list (for/list ([i input-list-ecne]) (- i 1))) ; translate back to 0-based index

    (define ostart 2) ; inclusive
    (define oend (+ 1 (header-section-npubout hs0))) ; inclusive
    (define output-list-ecne (for/list ([i (range ostart (+ 1 oend))]) i))
    (define output-list (for/list ([i output-list-ecne]) (- i 1))) ; translate back to 0-based index

    ; return
    (r1cs magic-number version nsec hs0 cs0 ws0 input-list output-list)
)

; returns a human readable string of one specified equation
; no mode is shown for better reading
(define (r1cs->string arg-r1cs arg-id)
    (define w2l (w2l-section-v (r1cs-w2l arg-r1cs))) ; w2l mapping, a list
    (define clist (constraint-section-constraints (r1cs-constraint arg-r1cs))) ; a list of constraints

    (define example-constraint (list-ref clist arg-id)) ; a constraint
    (define example-block-a (constraint-a example-constraint))
    (define example-block-b (constraint-b example-constraint))
    (define example-block-c (constraint-c example-constraint))

    ; process block a
    (define nnz-a (constraint-block-nnz example-block-a))
    (define wids-a (constraint-block-wids example-block-a))
    (define factors-a (constraint-block-factors example-block-a))
    (define str-a (string-join
        (for/list ([w0 wids-a] [f0 factors-a])
            (string-join (list
                "("
                (number->string f0)
                " * x"
                (number->string w0)
                ")"
            ) "")
        )
        " + "
    ))

    ; process block b
    (define nnz-b (constraint-block-nnz example-block-b))
    (define wids-b (constraint-block-wids example-block-b))
    (define factors-b (constraint-block-factors example-block-b))
    (define str-b (string-join
        (for/list ([w0 wids-b] [f0 factors-b])
            (string-join (list
                "("
                (number->string f0)
                " * x"
                (number->string w0)
                ")"
            ) "")
        )
        " + "
    ))

    ; process block c
    (define nnz-c (constraint-block-nnz example-block-c))
    (define wids-c (constraint-block-wids example-block-c))
    (define factors-c (constraint-block-factors example-block-c))
    (define str-c (string-join
        (for/list ([w0 wids-c] [f0 factors-c])
            (string-join (list
                "("
                (number->string f0)
                " * x"
                (number->string w0)
                ")"
            ) "")
        )
        " + "
    ))

    (string-join 
        (list 
            "( "
            (if (zero? (string-length str-a)) "0" str-a)
            " ) * ( "
            (if (zero? (string-length str-b)) "0" str-b)
            " ) = "
            (if (zero? (string-length str-c)) "0" str-c)
        ) 
    "")

)
