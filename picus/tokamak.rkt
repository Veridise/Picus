#lang rosette
(require
    (prefix-in ext: "./extensions.rkt")
)
(provide (rename-out
    [println-and-exit exit]
    [println-and-error error]
    [println-and-log log]
    [assert-type typed]
    [fresh-symbolic-variable* symbolic*]
    [decomposable? decomposable?]
    [concrete-integer? cinteger?]
    [concrete-natural? cnatural?]
))

(define (any? x) #t) ; any type
(define (concrete-integer? x) (and (concrete? x) (integer? x)))
(define (concrete-natural? x) (and (concrete-integer? x) (>= x 0)))

; used for forced break out of the execution
(define (println-and-exit msg . fmts)
    (eprintf (format "[tokamak:exit] ~a\n" (apply format (cons msg fmts))))
    (exit 0)
)

; used for throwing tokamak error with message
(define (println-and-error msg . fmts)
    (error (format "[tokamak:error] ~a\n" (apply format (cons msg fmts))))
    (error error) ; intended error for trace to locate the cause
    ; (exit 0)
)

(define (println-and-log msg . fmts)
    ;; TODO/fixme
    (eprintf (format "[tokamak:log] ~a\n" (apply format (cons msg fmts))))
)

; usually for debugging, asserting obj is one of types, otherwise print and exit
; typs is a list of type predicates
; (note) as long as there's a path that returns false **for all typs**, the exit function will be triggered
(define (assert-type-helper obj typs)
    (if (null? typs)
        #f
        (if ((car typs) obj)
            #t
            (assert-type-helper obj (cdr typs))
        )
    )
)
(define (assert-type obj . typs)
    (if (assert-type-helper obj typs)
        #t
        (println-and-exit "type checking failed, required types are: ~a, obj is: ~v" typs obj)
    )
)

; ref: https://github.com/emina/rosette/blob/master/rosette/base/form/define.rkt#L37
; arg-id and arg-type should beoth be symbols
(define (fresh-symbolic-variable* arg-id arg-type)
    (assert-type arg-id symbol?)
    (assert-type arg-type symbol?)
    
    (define tmp-type 
        (cond
            [(equal? 'bv256 arg-type) (bitvector 256)]
            [else (println-and-exit "unknown symbolic type, got: ~a" arg-type)]
        )
    )
    (define tmp-var (constant (list arg-id (ext:index!)) tmp-type))
    tmp-var
)

; (fixme) this function may be wrong, since in a term the op is an actual function, not a symbol
;         so you can't directly call (equal? 'ite op) to detect it, but you should use (equal? ite op)
;         where ite needs to be exported from rosette
; decide whether the given value can be decomposed by `for/all`
; (note) decomposible here means whether `for/all` will reveal new values or not, see comments for more details
(define (decomposable? v)
    (if (symbolic? v)
        ; symbolic
        (cond 
            ; a union is decomposable
            [(union? v) #t]

            ; symbolic constant is not decomposable
            [(constant? v) #f]

            ; for expression, it could be `ite` or other forms (e.g., +/-)
            ; we use reflecting provided publicly by rosette rather than hacking
            [(expression? v)
                (match v
                    ; (fixme) there are more builtin rosette operators that you need to consider
                    ;         e.g., bitvector->integer
                    [(expression op child ...) (or
                        (equal? 'ite* (object-name op))
                        (equal? 'ite (object-name op))
                    )]
                    [_ (println-and-exit "# [exception] you can't reach here.")] ; for debugging in case anyone overrides `expression`
                )
            ]

            ; (note) (important) this category usually corresponds to a collection that contains symbolic values
            ;                    note that `symbolic?` is contagious, it mark a value as symbolic as long as any
            ;                    of its member is symbolic
            ; e.g., a struct instance with a symbolic member belongs to this category, and regarding decomposibility,
            ; since it's for deciding whether one should use `for/all` (and whether new values will be revealed under `for/all`),
            ; this category does not require decomposing because `for/all` reveals no new values
            [else #f]
        )
        ; not symbolic, so not decomposable
        #f
    )
)
