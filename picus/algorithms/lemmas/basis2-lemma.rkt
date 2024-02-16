#lang racket
; this implements the basis-2 lemma (similar to Ecne Rule 3/5):
;   if z = 2^0 * x0 + 2^1 * x1 + ... + 2^n * xn, and x0, x1, ..., xn are all in {0,1}
;   then if z is uniquely determined, so as x0, x1, ..., xn
; this requires p1cnsts
(require (prefix-in config: "../../config.rkt")
         (prefix-in r1cs: "../../r1cs/r1cs-grammar.rkt")
         "../../logging.rkt"
         "../../exit.rkt"
         "match-pattern.rkt")
(provide apply-lemma)

; recursively apply linear lemma
(define (apply-lemma ks us p1cnsts range-vec)
  (picus:log-progress "[basis2 lemma] starting propagation")
  (define-values (tmp-ks tmp-us) (process ks us p1cnsts range-vec))
  (define s0 (set-subtract tmp-ks ks))
  (cond
    [(set-empty? s0) (picus:log-debug "[basis2 lemma] nothing added")]
    [else (picus:log-debug "[basis2 lemma] adding ~e" s0)])
  ; apply once is enough, return
  (values tmp-ks tmp-us))

;; to be initialized later in the function update
(define basis2-seqs #f)

(define (ffsub v) (- config:p v))

(define (all-binary01? range-vec sids)
  (cond
    [(null? sids) #t]
    [else
     (define sid (car sids))
     (define sids-rest (cdr sids))
     (if (set? (vector-ref range-vec sid))
         ; it's a set, then chec set value
         (cond
           [(or (equal? (set 0 1) (vector-ref range-vec sid))
                (equal? (set 0) (vector-ref range-vec sid))
                (equal? (set 1) (vector-ref range-vec sid)))
            (all-binary01? range-vec sids-rest)]
           [else #f])
         ; not a set, then no
         #f)]))

(define (process ks us arg-r1cs range-vec)
  (for/fold ([ks ks] [us us]) ([obj (in-list (r1cs:rcmds-vs arg-r1cs))])
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
                       (r1cs:radd (list-no-order-known-first
                                   #:pred r1cs:rvar?
                                   #:match (r1cs:rvar x0)
                                   #:else (r1cs:rmul (list-no-order-known-first
                                                      #:pred (match-lambda
                                                               [(or (r1cs:rvar "ps1")
                                                                    (r1cs:rvar "ps2")
                                                                    (r1cs:rvar "ps4")
                                                                    (r1cs:rint _)) #t]
                                                               [_ #f])
                                                      #:match vs
                                                      #:else (r1cs:rvar xs))) ...))
                       _)))
       ; (fixme) vs could be matched to x?? since it's not typed in the pattern
       ;         need a procedure to adjust this
       ; (printf "matched.\n")
       ; (when (equal? x0 "x2059")
       ;     (printf "x2059 matched.\n")
       ; (printf "vs: ~a\n" vs)
       ; (printf "xs: ~a\n" xs)
       ; )
       (update ks us x0 vs xs range-vec)]
      ; flip
      [(r1cs:rassert (r1cs:req
                      (r1cs:rmod
                       (r1cs:radd (list-no-order-known-first
                                   #:pred r1cs:rvar?
                                   #:match (r1cs:rvar x0)
                                   #:else (r1cs:rmul (list-no-order-known-first
                                                      #:pred (match-lambda
                                                               [(or (r1cs:rvar "ps1")
                                                                    (r1cs:rvar "ps2")
                                                                    (r1cs:rvar "ps4")
                                                                    (r1cs:rint _)) #t]
                                                               [_ #f])
                                                      #:match vs
                                                      #:else (r1cs:rvar xs))) ...))
                       _)
                      (r1cs:rvar "zero")))
       (update ks us x0 vs xs range-vec)]

      ; ==============================
      ; ==== finite field version ====
      ; ==============================
      [(r1cs:rassert (r1cs:req
                      (r1cs:rvar "zero")
                      (r1cs:radd (list-no-order-known-first
                                  #:pred r1cs:rvar?
                                  #:match (r1cs:rvar x0)
                                  #:else (r1cs:rmul (list-no-order-known-first
                                                     #:pred (match-lambda
                                                              [(or (r1cs:rvar "ps1")
                                                                   (r1cs:rvar "ps2")
                                                                   (r1cs:rvar "ps4")
                                                                   (r1cs:rint _)) #t]
                                                              [_ #f])
                                                     #:match vs
                                                     #:else (r1cs:rvar xs))) ...))))
       (update ks us x0 vs xs range-vec)]
      ; flip
      [(r1cs:rassert (r1cs:req
                      (r1cs:radd (list-no-order-known-first
                                  #:pred r1cs:rvar?
                                  #:match (r1cs:rvar x0)
                                  #:else (r1cs:rmul (list-no-order-known-first
                                                     #:pred (match-lambda
                                                              [(or (r1cs:rvar "ps1")
                                                                   (r1cs:rvar "ps2")
                                                                   (r1cs:rvar "ps4")
                                                                   (r1cs:rint _)) #t]
                                                              [_ #f])
                                                     #:match vs
                                                     #:else (r1cs:rvar xs))) ...))
                      (r1cs:rvar "zero")))
       (update ks us x0 vs xs range-vec)]

      ; otherwise, do not rewrite
      [_ (values ks us)])))

(define (update ks us x0 vs xs range-vec)
  (unless basis2-seqs
    (set! basis2-seqs
          (for/set ([i (range (floor (log config:p 2)))])
            (for/set ([j (range (+ 1 i))])
              (expt 2 j)))))

  ; extract coefficients
  ; need to remap by calling p-v since they are all in form of pv
  ; when moved to the other side they become v
  (define coelist
    (for/list ([v vs])
      (match v
        [(r1cs:rvar "ps1") (- config:p 1)]
        [(r1cs:rvar "ps2") (- config:p 2)]
        [(r1cs:rvar "ps4") (- config:p 4)]
        [(r1cs:rint z) z]
        [_ (picus:tool-error "unsupported coefficient, got: ~a" v)])))
  (define coelist2
    (for/list ([v vs])
      (match v
        [(r1cs:rvar "ps1") (ffsub (- config:p 1))]
        [(r1cs:rvar "ps2") (ffsub (- config:p 2))]
        [(r1cs:rvar "ps4") (ffsub (- config:p 4))]
        [(r1cs:rint z) (ffsub z)]
        [_ (picus:tool-error "unsupported coefficient, got: ~a" v)])))
  (define coeset (list->set coelist))
  (define coeset2 (list->set coelist2))

  (cond
    [(and (= (length coelist) (set-count coeset))
          ; there are no duplicate numbers, can apply basis lemma
          ; (note) duplicate variables are acceptable, but duplicate bases ar enot
          (or (set-member? basis2-seqs coeset) (set-member? basis2-seqs coeset2))
          ; yes it's a basis sequence
          ; check for signal ranges
          (all-binary01? range-vec xs)
          ; good, all binary01
          ; check if the target signal is already unique
          (set-member? ks x0)
          ; yes it's unique, then add all basis signals to known set
          )
     (define sigset (list->set xs))
     (values (set-union ks sigset)
             (set-subtract us sigset))]
    [else (values ks us)]))
