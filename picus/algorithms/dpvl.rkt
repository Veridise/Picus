#lang racket
; this implements the decide & propagate verification loop algorithm
(require csv-reading
         (prefix-in r1cs: "../r1cs/r1cs-grammar.rkt")
         (prefix-in selector: "./selector.rkt")
         (prefix-in solver: "../solver.rkt")
         ; lemmas
         (prefix-in l0: "./lemmas/linear-lemma.rkt")
         (prefix-in l1: "./lemmas/binary01-lemma.rkt")
         (prefix-in l2: "./lemmas/basis2-lemma.rkt")
         (prefix-in l3: "./lemmas/aboz-lemma.rkt")
         (prefix-in l4: "./lemmas/bim-lemma.rkt")
         ; (prefix-in ln0: "./lemmas/baby-lemma.rkt")
         "../logging.rkt"
         "../exit.rkt")
(provide apply-algorithm
         current-selector
         current-solver)

; ======== module global variables ======== ;

(define current-selector (make-parameter selector:counter))
(define current-solver (make-parameter solver:cvc5))

; problem pack, needs to be set and initialized by apply- function
(define :nwires null)
(define :input-set null)
(define :output-set null)
(define :target-set null)

(define :varlist null)
(define :varvec null)
(define :opts null)
(define :defs null)
(define :cnsts null) ; standard form
(define :p1cnsts null) ; normalized form optimized by phase 1 optimization

(define :alt-varlist null)
(define :alt-varvec null)
(define :alt-defs null)
(define :alt-cnsts null)
(define :alt-p1cnsts null)

(define :arg-prop null)
(define :arg-slv null)
(define :arg-timeout null)

(define :unique-set null)
(define :precondition null)

; optional arguments
(define :extcnsts null)
(define :skip-query? null)

; problem intermediate results
(define :partial-cmds null)

; more specific range for all signals, potential values are (from abstract to concrete):
;   - 'bottom: it's {0, ..., p-1} (everything)
;   - a set of potential values
; (note) this tracks the range, not the uniqueness status, they are different
(define :range-vec null)

(define :linear-clauses #f)
(define :weight-map #f)

; main solving procedure
; returns:
;   - (values 'verified info): the given query is verified
;   - (values 'sat info): the given query has a counter-example (not verified)
;   - (values 'skip info): the given query times out (small step)
(define (dpvl-solve ks us sid)
  (picus:log-progress "[solver] checking (~a, ~a)" (vector-ref :varvec sid) (vector-ref :alt-varvec sid))
  ; assemble commands
  (define known-cmds
    (r1cs:rcmds
     (for/list ([j ks])
       (r1cs:rassert (r1cs:req (r1cs:rvar (vector-ref :varvec j))
                               (r1cs:rvar (vector-ref :alt-varvec j)))))))
  (define final-cmds
    (r1cs:rcmds-append
     :partial-cmds
     (r1cs:rcmds (list
                  (r1cs:rcmt "=============================")
                  (r1cs:rcmt "======== known block ========")
                  (r1cs:rcmt "=============================")))
     known-cmds
     (r1cs:rcmds (list
                  (r1cs:rcmt "=============================")
                  (r1cs:rcmt "======== query block ========")
                  (r1cs:rcmt "=============================")))
     (if :skip-query?
         ; skip query
         (r1cs:rcmds (list
                      (r1cs:rcmt "signal query is skipped")
                      (r1cs:rsolve)))
         ; do not skip query
         (r1cs:rcmds (list
                      (r1cs:rassert (r1cs:rneq (r1cs:rvar (vector-ref :varvec sid)) (r1cs:rvar (vector-ref :alt-varvec sid))))
                      (r1cs:rsolve))))))
  ; perform optimization
  (define final-str (send (current-solver) encode-smt (r1cs:rcmds-append (r1cs:rcmds :opts) final-cmds)))
  (define res (send (current-solver) solve final-str :arg-timeout))
  (define solved? (cond
                    [(equal? 'unsat (car res))
                     (picus:log-progress "[solver] verified")
                     ; verified, safe
                     'verified]
                    [(equal? 'sat (car res))
                     (cond
                       ; skipping query, whatever sat is good
                       [:skip-query?
                        (picus:log-progress "[solver] sat (no query)")
                        'sat]
                       ; not skipping query, need to tell variable
                       ; (important) here if the current signal is not a target, it's ok to see a sat
                       [(set-member? :target-set sid)
                        ; the current signal is a target, now there's a counter-example, unsafe
                        ; in pp, this counter-example is valid
                        (picus:log-progress "[solver] sat")
                        'sat]
                       [else
                        ; not a target, fine, just skip
                        (picus:log-progress "[solver] sat but not a target")
                        'skip])]
                    [else
                     (picus:log-progress "[solver] skipped")
                     ; possibly timeout in small step, result is unknown
                     'skip]))
  (values solved? (cdr res)))

(define total-cpu 0)
(define total-real 0)
(define total-gc 0)

; select and solve
; returns:
;   - (values 'normal ks us info)
;   - (values 'break ks us info)
;   (note) since it's called recursively, at some level it could have new different ks with 'break
;          in that case you still break since a counter-example is already found
; uspool is usually initialized as us
(define (dpvl-select-and-solve ks us uspool)
  (cond
    [(set-empty? uspool)
     ; can't solve any more signal in this iteration
     (values 'normal ks us null)]
    ; else, set not empty, move forward
    [else
     (define sid (send (current-selector) apply-selector uspool :weight-map))
     (match-define-values
      ((list solved? info) cpu real gc)
      (time-apply (λ () (dpvl-solve ks us sid)) '()))
     (set! total-cpu (+ total-cpu cpu))
     (set! total-real (+ total-real real))
     (set! total-gc (+ total-gc gc))
     ; send feedback to selector
     (send (current-selector) selector-feedback sid solved?)
     (cond
       ; solved, update ks & us, then return
       [(equal? 'verified solved?) (values 'normal (set-add ks sid) (set-remove us sid) null)]
       ; found a counter-example here, forced stop, nothing more to solve
       ; return the same ks & us to indicate the caller to stop
       [(equal? 'sat solved?) (values 'break ks us info)]
       ; unknown or timeout, update uspool and recursively call again
       [(equal? 'skip solved?) (dpvl-select-and-solve ks us (set-remove uspool sid))]
       [else (picus:tool-error "unsupported solved? value, got: ~a" solved?)])]))

; (define tmp-inspect (list 512 513 514 515 516 517 518 519 384 385 386 387 388 389 390 391 392 393 394 395 396 397 398 399 400 401 402 403 404 405 406 407 408 409 410 411 412 413 414 415 416 417 418 419 420 421 422 423 424 425 426 427 428 429 430 431 432 433 434 435 436 437 438 439 440 441 442 443 444 445 446 447 448 449 450 451 452 453 454 455 456 457 458 459 460 461 462 463 464 465 466 467 468 469 470 471 472 473 474 475 476 477 478 479 480 481 482 483 484 485 486 487 488 489 490 491 492 493 494 495 496 497 498 499 500 501 502 503 504 505 506 507 508 509 510 511))
; (define tmp-inspect (list 1024 1025 1026 1027 1028 1029 1030 1031 896 897 898 899 900 901 902 903 904 905 906 907 908 909 910 911 912 913 914 915 916 917 918 919 920 921 922 923 924 925 926 927 928 929 930 931 932 933 934 935 936 937 938 939 940 941 942 943 944 945 946 947 948 949 950 951 952 953 954 955 956 957 958 959 960 961 962 963 964 965 966 967 968 969 970 971 972 973 974 975 976 977 978 979 980 981 982 983 984 985 986 987 988 989 990 991 992 993 994 995 996 997 998 999 1000 1001 1002 1003 1004 1005 1006 1007 1008 1009 1010 1011 1012 1013 1014 1015 1016 1017 1018 1019 1020 1021 1022 1023))
; (define tmp-inspect (list  2048 2049 2050 2051 2052 2053 2054 2055 2056 2057 2058 1805 1806 1807 1808 1809 1810 1811 1812 1813 1814 1815 1816 1817 1818 1819 1820 1821 1822 1823 1824 1825 1826 1827 1828 1829 1830 1831 1832 1833 1834 1835 1836 1837 1838 1839 1840 1841 1842 1843 1844 1845 1846 1847 1848 1849 1850 1851 1852 1853 1854 1855 1856 1857 1858 1859 1860 1861 1862 1863 1864 1865 1866 1867 1868 1869 1870 1871 1872 1873 1874 1875 1876 1877 1878 1879 1880 1881 1882 1883 1884 1885 1886 1887 1888 1889 1890 1891 1892 1893 1894 1895 1896 1897 1898 1899 1900 1901 1902 1903 1904 1905 1906 1907 1908 1909 1910 1911 1912 1913 1914 1915 1916 1917 1918 1919 1920 1921 1922 1923 1924 1925 1926 1927 1928 1929 1930 1931 1932 1933 1934 1935 1936 1937 1938 1939 1940 1941 1942 1943 1944 1945 1946 1947 1948 1949 1950 1951 1952 1953 1954 1955 1956 1957 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025 2026 2027 2028 2029 2030 2031 2032 2033 2034 2035 2036 2037 2038 2039 2040 2041 2042 2043 2044 2045 2046 2047))
; (define tmp-inspect (list 1806))

; recursively apply all lemmas until fixed point
(define (dpvl-propagate ks us)
  ; (printf "range-vec[inspect]: ~a\n" (for/list ([i tmp-inspect]) (vector-ref :range-vec i)))

  (define tmp-ks (list->set (set->list ks)))
  (define tmp-us (list->set (set->list us)))

  ; apply lemma 0: linear lemma
  (set!-values (:linear-clauses tmp-ks tmp-us) (l0:apply-lemma :linear-clauses tmp-ks tmp-us))

  ; apply lemma 1: binary01 lemma
  (set!-values (tmp-ks tmp-us) (l1:apply-lemma tmp-ks tmp-us :p1cnsts :range-vec))

  ; apply lemma 2: basis2 lemma
  (set!-values (tmp-ks tmp-us) (l2:apply-lemma tmp-ks tmp-us :p1cnsts :range-vec))

  ; apply lemma 3: all-but-one-zero
  (set!-values (tmp-ks tmp-us) (l3:apply-lemma tmp-ks tmp-us :p1cnsts :range-vec))

  ; apply lemma 4: big-int-mul
  (set!-values (tmp-ks tmp-us) (l4:apply-lemma tmp-ks tmp-us :p1cnsts :range-vec))

  ; ; apply lemma ln0
  ; (set!-values (tmp-ks tmp-us) (ln0:apply-lemma tmp-ks tmp-us :p1cnsts))

  ; return
  (if (= (set-count ks) (set-count tmp-ks))
      ; no updates, return
      (values tmp-ks tmp-us)
      ; has updates, call again
      (dpvl-propagate tmp-ks tmp-us)
      )
  )

; perform one iteration of pp algorithm
;   - ks: known set
;   - us: unknown set
; returns:
;   - ('safe ks us info)
;   - ('unsafe ks us info)
;   - ('unknown ks us info)
(define (dpvl-iterate ks us)

  ; first, propagate
  (define-values (new-ks new-us) (if :arg-prop
                                     ; do propagation
                                     (dpvl-propagate ks us)
                                     ; don't do propagation
                                     (values ks us)))
  (cond
    [(set-empty? (set-intersect :target-set new-us))
     ; no target signal is unknown, no need to solve any more, return
     (values 'safe new-ks new-us null)]
    [else
     ; still there's unknown target signal, continue
     ; then select and solve
     (define-values (s0 xnew-ks xnew-us info)
       (if :arg-slv
           ; do solver phase
           (dpvl-select-and-solve new-ks new-us new-us)
           ; don't do solver phase
           (values 'normal new-ks new-us null)))
     (cond
       ; normal means there's no counter-example
       [(equal? 'normal s0)
        (cond
          [(set-empty? (set-intersect :target-set xnew-us))
           ; no target signal is unknown, return
           (values 'safe xnew-ks xnew-us null)]
          [(equal? xnew-us new-us)
           ; can't reduce any unknown any more, return
           (values 'unknown xnew-ks xnew-us info)]
          [else
           ; continue the iteration
           (dpvl-iterate xnew-ks xnew-us)])]
       ; 'break means there's counter-example
       [(equal? 'break s0) (values 'unsafe xnew-ks xnew-us info)]
       [else (picus:tool-error "unsupported s0 value, got: ~a" s0)])]))

; this creates a new hash with r1cs variables replaced by corresponding circom variables
; (note) when unmappable? is #f, this will remove helping variables like "one", "ps?", etc.
;; map-to-vars :: (listof hash?), path-string? -> (listof hash?)
(define (map-to-vars info path-sym)
  (define rd (call-with-input-file* path-sym (λ (port) (csv->list port))))
  ; create r1cs-id to circom-var mapping
  (define r2c-map
    (make-hash (for/list ([p rd]) (cons (list-ref p 0) (list-ref p 3)))))
  (define (process-subinfo pinfo)
    (for/list ([pair (in-list pinfo)]
               #:do [(match-define (cons k val) pair)])
      (cons (format "~a" (hash-ref r2c-map (number->string k))) val)))
  (map process-subinfo info))

;; partition-vars :: (or/c '() hash?), set?, set? -> (list/c hash? hash? hash? hash? hash?)
(define (partition-vars info input-set output-set)
  (define pinfo (if (list? info) (make-hash) info)) ; patch for info type, fix later
  (for/fold ([input-info '()]
             [output1-info '()]
             [output2-info '()]
             [other1-info '()]
             [other2-info '()]
             #:result (list input-info output1-info output2-info other1-info other2-info))
            ([(k v) (in-hash pinfo)])
    (match k
      ;; matching for a variable in the input set. By construction,
      ;; this variable is in the form x<number-id>.
      [(pregexp #px"^x(\\d+)$" (list _ (app string->number n)))
       #:when (set-member? input-set n)
       (values (cons (cons n v) input-info)
               output1-info
               output2-info
               other1-info
               other2-info)]
      ;; skip y<number-id> when <number-id> is in the input set,
      ;; since it's the same as x<number-id>
      [(pregexp #px"^y(\\d+)$" (list _ (app string->number n)))
       #:when (set-member? input-set n)
       (values input-info
               output1-info
               output2-info
               other1-info
               other2-info)]
      ;; matching for a variable in the output set. By construction,
      ;; this variable is in the form x<number-id> or y<number-id>,
      ;; where x indicates that it is in the first set
      ;; and y indicates that it is in the second set.
      [(pregexp #px"^(?:x|y)(\\d+)$" (list _ (app string->number n)))
       #:when (set-member? output-set n)
       (values input-info
               (if (string-prefix? k "x") (cons (cons n v) output1-info) output1-info)
               (if (string-prefix? k "y") (cons (cons n v) output2-info) output2-info)
               other1-info
               other2-info)]
      ;; matching for internal signals. By construction,
      ;; this variable is in the form x<number-id> or y<number-id>,
      ;; where x indicates that it is in the first set
      ;; and y indicates that it is in the second set.
      [(pregexp #px"^(?:x|y)(\\d+)$" (list _ (app string->number n)))
       (values input-info
               output1-info
               output2-info
               (if (string-prefix? k "x") (cons (cons n v) other1-info) other1-info)
               (if (string-prefix? k "y") (cons (cons n v) other2-info) other2-info))]

      ;; matching for symbolic variables inserted by Picus
      [(pregexp #px"^(?:ps\\d|zero|one|p)$")
       (values input-info
               output1-info
               output2-info
               other1-info
               other2-info)]

      [_ (picus:tool-error "unexpected variable: ~a" k)])))

;; sort variables according to their signal id
;; We do this since it's an easy way to sort array entries properly.
;; Had we use naive string comparison on the variable names,
;; we would have "x[11]" < "x[2]"
(define (sort-vars info)
  (map (λ (subinfo) (sort subinfo < #:key car)) info))

; verifies signals in target-set
; returns (same as dpvl-iterate):
;   - (values 'safe ks us info)
;   - (values 'unsafe ks us info)
;   - (values 'unknown ks us info)
(define (apply-algorithm
         nwires
         input-set output-set target-set
         varlist opts defs cnsts
         alt-varlist alt-defs alt-cnsts
         unique-set precondition
         arg-prop arg-slv arg-timeout path-sym
         ; extra constraints, usually from cex module about partial model
         #:extcnsts [extcnsts (r1cs:rcmds (list ))]
         ; if true, then the query block will not be issued
         ; this is required for the cex module to finalize a non-relevant part, which only requires a trivial model
         #:skip-query? [skip-query? #f])

  (set! total-cpu 0)
  (set! total-real 0)
  (set! total-gc 0)

  ; first load in all global variables
  (set! :nwires nwires)
  (set! :input-set input-set)
  (set! :output-set output-set)
  (set! :target-set target-set)

  (set! :varlist varlist)
  (set! :varvec (list->vector varlist))
  (set! :opts opts)
  (set! :defs defs)
  (set! :cnsts cnsts)

  (set! :alt-varlist alt-varlist)
  (set! :alt-varvec (list->vector alt-varlist))
  (set! :alt-defs alt-defs)
  (set! :alt-cnsts alt-cnsts)

  (set! :arg-prop arg-prop)
  (set! :arg-slv arg-slv)
  (set! :arg-timeout arg-timeout)

  (set! :unique-set unique-set)
  (set! :precondition precondition)

  ; optional arguments
  (set! :extcnsts extcnsts)
  (set! :skip-query? skip-query?)

  (define known-set
    (for/set ([i (in-range :nwires)]
              #:when (set-member? :input-set i))
      i))

  (define unknown-set
    (for/set ([i (in-range :nwires)]
              #:unless (set-member? :input-set i))
      i))


  (picus:log-debug "initial known-set ~e" known-set)
  (picus:log-debug "initial unknown-set ~e" unknown-set)
  
  ; (precondition related) incorporate unique-set if unique-set is not an empty set
  (set! known-set (set-union known-set unique-set))
  (set! unknown-set (set-subtract unknown-set unique-set))
  (picus:log-debug "refined known-set: ~e" known-set)
  (picus:log-debug "refined unknown-set: ~e" unknown-set)

  ; ==== branch out: skip optimization phase 0 and apply expand & normalize ====
  ; computing rcdmap need no ab0 lemma from optimization phase 0
  (define :sdmcnsts
    (send (current-solver) normalize-r1cs
          (send (current-solver) expand-r1cs :cnsts)))

  ; generate linear-clauses requires no optimization to exclude ror and rand
  ; linear-clauses requires normalized constraints to get best results
  (set! :linear-clauses (l0:compute-linear-clauses :sdmcnsts #t))
  (set! :weight-map (l0:compute-weight-map :linear-clauses))

  ; ==== first apply optimization phase 0 ====
  (define :p0cnsts (send (current-solver) optimize-r1cs-p0 :cnsts))
  (define :alt-p0cnsts (send (current-solver) optimize-r1cs-p0 :alt-cnsts))

  ; ==== then expand the constraints ====
  (define :expcnsts (send (current-solver) expand-r1cs :p0cnsts))
  (define :alt-expcnsts (send (current-solver) expand-r1cs :alt-p0cnsts))

  ; ==== then normalize the constraints ====
  (define :nrmcnsts (send (current-solver) normalize-r1cs :expcnsts))
  (define :alt-nrmcnsts (send (current-solver) normalize-r1cs :alt-expcnsts))

  ; ==== then apply optimization phase 1 ====
  (set! :p1cnsts (send (current-solver) optimize-r1cs-p1 :nrmcnsts))
  (set! :alt-p1cnsts (send (current-solver) optimize-r1cs-p1 :alt-nrmcnsts))

  ; prepare partial cmds for better reuse through out the algorithm
  (set! :partial-cmds
        (r1cs:rcmds-append
         (r1cs:rcmds (list
                      (r1cs:rcmt "================================")
                      (r1cs:rcmt "======== original block ========")
                      (r1cs:rcmt "================================")))
         :defs
         (send (current-solver) get-pdefs)
         (r1cs:rcmds (list (r1cs:rcmt "======== main constraints ========")))
         :p1cnsts
         (r1cs:rcmds (list (r1cs:rassert (r1cs:req (r1cs:rint 1) (r1cs:rvar (format "x0"))))))
         (r1cs:rcmds (list
                      (r1cs:rcmt "===================================")
                      (r1cs:rcmt "======== alternative block ========")
                      (r1cs:rcmt "===================================")))
         :alt-defs
         (r1cs:rcmds (list (r1cs:rcmt "======== main constraints ========")))
         :alt-p1cnsts
         (r1cs:rcmds (list (r1cs:rassert (r1cs:req (r1cs:rint 1) (r1cs:rvar (format "y0"))))))
         (r1cs:rcmds (list
                      (r1cs:rcmt "====================================")
                      (r1cs:rcmt "======== precondition block ========")
                      (r1cs:rcmt "====================================")))
         (if (null? :precondition)
             ; no precondition
             (r1cs:rcmds (list (r1cs:rcmt "(no precondition or skipped by user)")))
             ; assemble precondition
             (r1cs:rcmds (flatten (for/list ([v :precondition])
                                    (cons
                                     (r1cs:rcmt (format "precondition tag: ~a" (car v)))
                                     (cdr v))))))
         (r1cs:rcmds (list
                      (r1cs:rcmt "========================================")
                      (r1cs:rcmt "======== extra constraint block ========")
                      (r1cs:rcmt "========================================")))
         extcnsts))

  ; initialize range set to all values
  (set! :range-vec (build-vector :nwires (lambda (x) 'bottom)))
  ; x0 is always 1
  (vector-set! :range-vec 0 (list->set (list 1)))

  ; invoke the algorithm iteration
  (define-values (ret0 rks rus info) (dpvl-iterate known-set unknown-set))

  (picus:log-accounting #:type "solving_time_cpu"
                        #:value total-cpu
                        #:unit "ms"
                        #:msg "Time spent for solving (cpu)")
  (picus:log-accounting #:type "solving_time_real"
                        #:value total-real
                        #:unit "ms"
                        #:msg "Time spent for solving (real)")
  (picus:log-accounting #:type "solving_time_gc"
                        #:value total-gc
                        #:unit "ms"
                        #:msg "Time spent for solving (gc)")

  ; always skip x0 and y0, since they are hard-coded to 1 in the algorithm, but
  ; the actual value here might be different, which could be misleading.
  (when (hash? info)
    (hash-remove! info "x0")
    (hash-remove! info "y0"))

  (define partitioned-info (sort-vars (partition-vars info input-set output-set)))

  (unless (file-exists? path-sym)
    (picus:log-warning "~a does not exist" path-sym))

  (define mapped-info
    (cond
      [(file-exists? path-sym) (map-to-vars partitioned-info path-sym)]
      [else partitioned-info]))

  (values ret0 rks rus mapped-info partitioned-info))
