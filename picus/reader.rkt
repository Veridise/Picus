#lang racket/base

(provide r1cs
         circom)

(require racket/class
         racket/list
         racket/file
         racket/system
         racket/format
         racket/path
         racket/match
         csv-reading
         (prefix-in r1cs: "r1cs/r1cs-grammar.rkt")
         "tmpdir.rkt"
         "logging.rkt"
         "exit.rkt"
         "gen-witness.rkt")

(define r1cs-interface<%>
  (interface ()
    validate ;; (-> any/c void?)
    get-source ;; (-> string?)
    get-format ;; (-> string?)
    get-num-wires ;; (-> natural?)
    get-num-constraints ;; (-> natural?)
    get-prime-number ;; (-> prime?)
    get-field-size ;; (-> (or/c #f natural?))
    get-top-level-inputs ;; (-> (listof natural?))
    get-top-level-outputs ;; (-> (listof natural?))
    get-constraints ;; (-> (listof r1cs:constraint?))

    ;; NOTE: to be implemented further
    get-extra-constraints ;; (-> (listof any/c))

    map-to-vars ;; (-> (listof pair?) (listof pair?))
    gen-witness-files ;; (-> info? void?)
    ))

(define (invoke-system cmd . args)
  (define outp (open-output-string))
  (define ret
    (parameterize ([current-output-port outp]
                   [current-error-port outp])
      (apply system* cmd args)))
  (values ret (get-output-string outp)))

(define r1cs%
  (class* object% (r1cs-interface<%>)
    (super-new)
    (init-field source
                source-format)

    (define r0 (r1cs:read-r1cs source))

    (define/public (validate _gen-witness)
      (void))

    (define/public (get-source)
      source)

    (define/public (get-format)
      source-format)

    (define/public (get-num-wires)
      (r1cs:get-nwires r0))

    (define/public (get-num-constraints)
      (r1cs:get-mconstraints r0))

    (define/public (get-prime-number)
      (r1cs:get-prime-number r0))

    (define/public (get-field-size)
      (r1cs:get-field-size r0))

    (define/public (get-top-level-inputs)
      (r1cs:r1cs-inputs r0))

    (define/public (get-top-level-outputs)
      (r1cs:r1cs-outputs r0))

    (define/public (get-constraints)
      (r1cs:get-constraints r0))

    (define/public (get-extra-constraints)
      '())

    ;; r2-map :: (or/c 'uninitialized 'not-found (hash/c string? string?))
    (define r2c-map 'uninitialized)

    (define/public (map-to-vars info)
      (match r2c-map
        ['uninitialized
         (define path-sym (path-replace-extension source ".sym"))
         (cond
           [(file-exists? path-sym)
            (define rd (call-with-input-file* path-sym (λ (port) (csv->list port))))
            ; create r1cs-id to circom-var mapping
            (set! r2c-map
                  (make-hash (for/list ([p rd]) (cons (list-ref p 0) (list-ref p 3)))))]
           [else
            (picus:log-warning "~a does not exist" path-sym)
            (set! r2c-map 'not-found)])
         ;; retry again after initialization
         (map-to-vars info)]
        ['not-found info]
        [_
         (for/list ([pair (in-list info)]
                    #:do [(match-define (cons k val) pair)])
           (cons (hash-ref r2c-map (number->string k)) val))]))

    (define/public (gen-witness-files raw-info)
      (gen-witness raw-info this))))

(define ((r1cs source) #:opt-level [opt-level #f])
  (when opt-level
    (picus:user-error "'--opt-level' only applicable for circom source"))

  (new r1cs%
       [source source]
       [source-format "r1cs"]))

(define ((circom source) #:opt-level [opt-level #f])
  (define-values (ret out)
    (invoke-system (find-executable-path "circom")
                   "-o"
                   (get-tmpdir)
                   "--r1cs"
                   source
                   "--sym"
                   (match opt-level
                     [(or "0" #f) "--O0"]
                     ["1" "--O1"]
                     ["2" "--O2"])))
  (cond
    [ret (picus:log-debug "circom output: ~a" out)]
    [else
     (picus:log-error "[circom] ~a" out)
     (picus:user-error "circom compilation failed")])

  (new r1cs%
       [source (~a (build-path
                    (get-tmpdir)
                    (file-name-from-path
                     (path-replace-extension source ".r1cs"))))]
       [source-format "circom"]))
