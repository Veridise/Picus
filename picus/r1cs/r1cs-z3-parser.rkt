#lang racket
; this interprets binary r1cs into its grammar representation
(require (prefix-in config: "../config.rkt")
         (prefix-in r1cs: "./r1cs-grammar.rkt"))
(provide parse-r1cs
         expand-r1cs)

; turns r1cs binary form into standard form
; arguments:
;   - arg-r1cs: r1cs binary form instance using the struct r1cs grammar
;   - prefix :: (or/c "x" "y")
; returns:
;   - (values varlist declarations constraints)
(define (parse-r1cs arg-r1cs)
  ; first create a list of all symbolic variables according to nwires
  (define nwires (send arg-r1cs get-num-wires))
  ; strictly align with wid
  (define varvec (for/vector ([i (in-range nwires)]) i))

  ; add range constraints for declared variables
  (define raw-decls
    (append (list (r1cs:rcmt "======== declaration constraints ========"))
            (for/list ([x (in-vector varvec)])
              (r1cs:rdef (r1cs:rvar x) (r1cs:rtype "Int")))
            (list (r1cs:rcmt "======== range constraints ========"))
            (for/list ([x (in-vector varvec)])
              (r1cs:rassert (r1cs:rand (list
                                        (r1cs:rleq (r1cs:rint 0) (r1cs:rvar x))
                                        (r1cs:rlt (r1cs:rvar x) (r1cs:rint config:p))))))))

  ; symbolic constraints
  (define sconstraints
    (for/list ([cnst (send arg-r1cs get-constraints)])

      ; get block
      (define curr-block-a (r1cs:constraint-a cnst))
      (define curr-block-b (r1cs:constraint-b cnst))
      (define curr-block-c (r1cs:constraint-c cnst))

      ; process block a
      (define wids-a (r1cs:constraint-block-wids curr-block-a))
      (define factors-a (r1cs:constraint-block-factors curr-block-a))

      ; process block b
      (define wids-b (r1cs:constraint-block-wids curr-block-b))
      (define factors-b (r1cs:constraint-block-factors curr-block-b))

      ; process block c
      (define wids-c (r1cs:constraint-block-wids curr-block-c))
      (define factors-c (r1cs:constraint-block-factors curr-block-c))

      ; ======== parse into original form: A*B = C ========
      ; note that terms could be empty, in which case 0 is used
      (define terms-a
        (cons (r1cs:rint 0)
              (for/list ([w0 wids-a] [f0 factors-a])
                ; (format "(* ~a ~a)" f0 x0)
                (let ([x0 (vector-ref varvec w0)])
                  (r1cs:rmul (list (r1cs:rint f0) (r1cs:rvar x0)))))))
      (define terms-b
        (cons (r1cs:rint 0)
              (for/list ([w0 wids-b] [f0 factors-b])
                ; (format "(* ~a ~a)" f0 x0)
                (let ([x0 (vector-ref varvec w0)])
                  (r1cs:rmul (list (r1cs:rint f0) (r1cs:rvar x0)))))))
      (define terms-c
        (cons (r1cs:rint 0)
              (for/list ([w0 wids-c] [f0 factors-c])
                ; (format "(* ~a ~a)" f0 x0)
                (let ([x0 (vector-ref varvec w0)])
                  (r1cs:rmul (list (r1cs:rint f0) (r1cs:rvar x0)))))))
      (define sum-a (r1cs:radd terms-a))
      (define sum-b (r1cs:radd terms-b))
      (define sum-c (r1cs:radd terms-c))
      ; original form: A*B = C
      (r1cs:rassert
       (r1cs:req
        (r1cs:rmod (r1cs:rmul (list sum-a sum-b)) (r1cs:rint config:p))
        (r1cs:rmod sum-c (r1cs:rint config:p))))))

  (values (vector->list varvec)
          (r1cs:rcmds raw-decls)
          (r1cs:rcmds sconstraints)))


; expands standard r1cs into terms++ form (sum of terms)
; usually only the cnsts part is provided
; arguments:
;   - arg-r1cs: r1cs standard form instance using the struct rcmds grammar
; returns:
;   - expanded-r1cs
(define (expand-r1cs arg-r1cs)
  (define ret-cnsts
    (for/list ([v (r1cs:rcmds-vs arg-r1cs)])
      (match v
        ; match the standard form a*b=c
        [(r1cs:rassert (r1cs:req
                        (r1cs:rmod
                         (r1cs:rmul (list
                                     (r1cs:radd (list (r1cs:rint 0) (r1cs:rmul (list (r1cs:rint a0s) (r1cs:rvar a1s))) ...)) ; shape of A
                                     (r1cs:radd (list (r1cs:rint 0) (r1cs:rmul (list (r1cs:rint b0s) (r1cs:rvar b1s))) ...)))) ; shape of B
                         _)
                        (r1cs:rmod
                         (r1cs:radd (list (r1cs:rint 0) (r1cs:rmul (list (r1cs:rint c0s) (r1cs:rvar c1s))) ...)) ; shape of C
                         _)))
         ; start the expansion
         (define terms-a (for/list ([a0 a0s][a1 a1s]) (cons a0 a1)))
         (define terms-b (for/list ([b0 b0s][b1 b1s]) (cons b0 b1)))
         (define terms-c (for/list ([c0 c0s][c1 c1s]) (cons c0 c1)))
         ; A*B = C but expand A*B into ++terms
         (r1cs:rassert (r1cs:req
                        ; A*B part
                        (if (or (empty? terms-a) (empty? terms-b))
                            ; A*B is empty
                            (r1cs:rint 0)
                            ; A*B is not empty
                            (r1cs:rmod
                             (r1cs:radd (for*/list ([va terms-a][vb terms-b])
                                          (r1cs:rmul (list
                                                      (r1cs:rint (remainder (* (car va) (car vb)) config:p))
                                                      (r1cs:rvar (cdr va))
                                                      (r1cs:rvar (cdr vb))))))
                             (r1cs:rint config:p)))
                        ; C part
                        (if (empty? terms-c)
                            ; C is empty
                            (r1cs:rint 0)
                            ; C is not empty
                            (r1cs:rmod
                             (r1cs:radd (for/list ([v terms-c]) (r1cs:rmul (list (r1cs:rint (car v)) (r1cs:rvar (cdr v))))))
                             (r1cs:rint config:p)))))]
        ; otherwise, just keep the form
        [_
         ; (tokamak:log "out of pattern: ~a" v)
         v])))
  (r1cs:rcmds ret-cnsts))
