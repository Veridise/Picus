#lang racket
; this implements the decide & propagate verification loop algorithm
(require graph
    (prefix-in tokamak: "../tokamak.rkt")
    (prefix-in utils: "../utils.rkt")
    (prefix-in config: "../config.rkt")
    (prefix-in r1cs: "../r1cs/r1cs-grammar.rkt")
    (prefix-in cg: "./constraint-graph.rkt")
    (prefix-in dpvl: "./dpvl.rkt")
)
(provide (rename-out
    [apply-algorithm apply-algorithm]
))

; convert a (partial) model to r1cs constraints (assignment assertions)
(define (model2cnsts arg-model)
    ; direct return
    (r1cs:rcmds (for/list ([key (hash-keys arg-model)])
        (r1cs:rassert (r1cs:req
            (r1cs:rvar key)
            (r1cs:rint (hash-ref arg-model key))
        ))
    ))
)

(define (apply-algorithm
    r0 nwires mconstraints
    input-set output-set target-set
    xlist opts defs cnsts
    alt-xlist alt-defs alt-cnsts
    unique-set precondition
    arg-selector arg-prop arg-timeout arg-smt path-sym
    solve interpret-r1cs
    parse-r1cs optimize-r1cs-p0 expand-r1cs normalize-r1cs optimize-r1cs-p1
    )

    (define cg0 (cg:compute-constraint-graph r0 cnsts path-sym))
    (define g0 (cg:cgraph-g cg0))
    (define edge2cid (cg:cgraph-e2c cg0))
    (define sid2order (cg:cgraph-s2o cg0))
    (define sid2scope (cg:cgraph-s2s cg0))
    (define order2scope (cg:cgraph-o2s cg0))
    (define norders (vector-length order2scope))

    (define partial-model (make-hash))

    (when (not (equal? (set "main") (vector-ref order2scope (- norders 1))))
        (tokamak:exit "tha last scope is not main"))

    ; exclude the main scope in iteration
    (for ([i (range (- norders 1))])
        (define iscope (vector-ref order2scope i)) ; current scope
        (define nscope (vector-ref order2scope (+ 1 i))) ; next scope, last scope is natually main
        (printf "===========================================\n")
        (printf "# i=~a, scope=~a\n" i iscope)
        (printf "===========================================\n")
        ; get connecting pairs
        (define icps (cg:get-connecting-pairs cg0 iscope))
        ; extract connected signals (outside scope) from connecting pairs
        (define icsigs (list->set (for/list ([p icps]) (cdr p))))
        ; extract subgraph with extra connected signals
        (define isubcg (cg:get-scoped-subgraph cg0 iscope icsigs))
        (define isubg0 (cg:cgraph-g isubcg))
        ; (printf "# subgraph:\n~a\n" (graphviz isubg0))

        ; decide local targets (main targets + signals connecting to next scope)
        ; which is also local target signals
        ; (fixme) probably need to check whether signal is already in partial model
        (define global-tsigs (list->set (filter
            (lambda (x) (and
                (equal? (set "main") (vector-ref sid2scope x)))
                (set-member? target-set x)
            )
            (set->list icsigs)
        )))
        (printf "# global-tsigs ~a\n" global-tsigs)
        (define local-tsigs (list->set (filter
            (lambda (x) (equal? nscope (vector-ref sid2scope x)))
            (set->list icsigs)
        )))
        (printf "# local-tsigs ~a\n" local-tsigs)
        (define query-tsigs (if (set-empty? global-tsigs)
            ; no global target signals connected to this scope, use local one
            local-tsigs
            ; there's global target signals, use global
            ; (fixme) this is not entirely correct
            global-tsigs
        ))
        (printf "# init query-tsigs ~a\n" query-tsigs)
        ; removed query signals that are already in the partial model
        (set! query-tsigs (list->set (filter
            (lambda (x) (not (hash-has-key? partial-model (format "x~a" x))))
            (set->list query-tsigs)
        )))
        (printf "# refined query-tsigs ~a\n" query-tsigs)
        ; all signals in the subgraph
        (define local-sigs (list->set (get-vertices isubg0)))
        (printf "# local-sigs ~a\n" local-sigs)
        ; all constraint ids in the subgraph
        (define local-cids (sort (remove-duplicates (flatten (for/list ([e (get-edges isubg0)])
            (set->list (hash-ref edge2cid (list->set e)))
        ))) <))
        (printf "# local-cids ~a\n" local-cids)

        (define icnsts (r1cs:rcmds (for/list ([cid local-cids]) (r1cs:rcmds-ref cnsts cid))))
        (define ialt-cnsts (r1cs:rcmds (for/list ([cid local-cids]) (r1cs:rcmds-ref alt-cnsts cid))))

        (printf "# start solving...\n")
        (define-values (res res-ks res-us res-info) (dpvl:apply-algorithm
            r0 nwires mconstraints
            input-set output-set query-tsigs
            xlist opts defs icnsts
            alt-xlist alt-defs ialt-cnsts
            unique-set precondition ; prior knowledge row
            arg-selector arg-prop #t arg-timeout arg-smt #f path-sym
            solve interpret-r1cs
            parse-r1cs optimize-r1cs-p0 expand-r1cs normalize-r1cs optimize-r1cs-p1
            #:extcnsts (model2cnsts partial-model)
            #:skip-query? (equal? i 2) ; (debug)
        ))

        ; no counter-example can be found within time limit, exit directly
        (when (not (equal? 'unsafe res))
            (tokamak:exit "# query returns: ~a, cannot continue counter-example generation" res))

        ; there's a counter-example, store partial model
        (define str-ids (remove-duplicates (append
            (for/list ([sid local-sigs]) (list-ref xlist sid))
            (for/list ([sid local-sigs]) (list-ref alt-xlist sid))
        )))
        (printf "# str-ids: ~a\n" str-ids)
        (for ([sid str-ids])
            (hash-set! partial-model sid (hash-ref res-info sid))
        )

        (printf "# end solving.\n")
        ; (todo) if success, need to update exclude-scopes
    )

    ; return
    (values 'unsafe null null partial-model)

)
