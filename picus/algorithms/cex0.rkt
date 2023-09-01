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

    (define n-orders (vector-length order2scope))
    ; (printf "# order2scope: ~a\n" order2scope)
    ; extra set to consider when extracting subgraph
    (define extra-set (set-union input-set output-set))
    (define exclude-scopes (set (set "main"))) ; starts with main scope
    (define partial-model (make-hash))

    (when (not (equal? (set "main") (vector-ref order2scope (- n-orders 1))))
        (tokamak:exit "tha last scope is not main"))

    (for ([i (range (- n-orders 1))])
        (define i-scope (vector-ref order2scope i))
        (printf "===========================================\n")
        (printf "# i=~a, scope=~a\n" i i-scope)
        (printf "===========================================\n")
        (define i-subcg (cg:get-scoped-subgraph cg0 i-scope extra-set #:touching? #t))
        (define i-subg0 (cg:cgraph-g i-subcg))
        ; (printf "# subgraph:\n~a\n" (graphviz (cg:cgraph-g i-subcg)))
        ; construct new scoped arguments
        ; (fixme) for now, we only get a subset of the following and do not change others:
        ;   - target-set
        ;   - cnsts
        ;   - alt-cnsts

        ; all signal ids in the current subgraph
        (define i-sids (list->set (get-vertices i-subg0)))
        ; extract local output signals (signals that connect to other scopes)
        (define lo-sids (cg:get-connecting-signals cg0 i-scope #:exclude-scopes exclude-scopes))
        ; (printf "# i-sids: ~a\n" i-sids)
        (define i-target-set (set-union lo-sids (set-intersect target-set i-sids)))
        (printf "# i-target-set: ~a\n" i-target-set)
        (define i-cids (sort (remove-duplicates (flatten (for/list ([e (get-edges i-subg0)])
            (set->list (hash-ref edge2cid (list->set e)))
        ))) <))
        (define i-cnsts (r1cs:rcmds (for/list ([cid i-cids]) (r1cs:rcmds-ref cnsts cid))))
        (define i-alt-cnsts (r1cs:rcmds (for/list ([cid i-cids]) (r1cs:rcmds-ref alt-cnsts cid))))
        (printf "# i-cids: ~a\n" i-cids)

        (printf "# start solving...\n")
        (define-values (res res-ks res-us res-info) (dpvl:apply-algorithm
            r0 nwires mconstraints
            input-set output-set i-target-set
            xlist opts defs i-cnsts
            alt-xlist alt-defs i-alt-cnsts
            unique-set precondition ; prior knowledge row
            arg-selector arg-prop arg-timeout arg-smt path-sym
            solve interpret-r1cs
            parse-r1cs optimize-r1cs-p0 expand-r1cs normalize-r1cs optimize-r1cs-p1
            #:extcnsts (model2cnsts partial-model)
        ))

        ; no counter-example can be found within time limit, exit directly
        (when (not (equal? 'unsafe res))
            (tokamak:exit "# query returns: ~a, cannot continue counter-example generation" res))

        ; there's a counter-example, store partial model
        (define str-ids (remove-duplicates (append
            (for/list ([sid i-sids]) (list-ref xlist sid))
            (for/list ([sid i-sids]) (list-ref alt-xlist sid))
        )))
        (printf "# str-ids: ~a\n" str-ids)
        (for ([sid str-ids])
            (hash-set! partial-model sid (hash-ref res-info sid))
        )

        (printf "# end solving.\n")

        ; (todo) if success, need to update exclude-scopes
    )

)
