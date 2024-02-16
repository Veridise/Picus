#lang racket/base

(provide r1cs
         circom
         sr1cs)

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

    ;; r2c-map :: (or/c 'uninitialized 'not-found (hash/c string? string?))
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

(define ((sr1cs source) #:opt-level [opt-level #f])
  (when opt-level
    (picus:user-error "'--opt-level' only applicable for circom source"))

  (define data (file->list source))

  (define input-list '())
  (define output-list '())
  (define num-wires #f)
  (define constraints '())
  (define extra-constraints '())
  (define prime #f)

  (define (first+fix xs)
    (define x (first xs))
    (if (< x 0) (+ x prime) x))

  (define (interp fml)
    (match fml
      [`(< ,a ,b) (r1cs:rlt (interp a) (interp b))]
      [`(var ,(? number? id))
       (r1cs:rvar id)]
      [`(int ,x) (r1cs:rint x)]))

  ;; r2c-map ::  (hash/c number? symbol?)
  (define r2c-map (make-hash))

  (for ([entry (in-list data)])
    (match entry
      [(list 'in (? number? id))
       (set! input-list (cons id input-list))]
      [(list 'out (? number? id))
       (set! output-list (cons id output-list))]
      [(list 'label (? number? id) (? symbol? label))
       (hash-set! r2c-map id label)]
      [(list 'num-wires val)
       (set! num-wires val)]
      [(list 'prime-number val)
       (set! prime val)]
      [(list 'extra-constraint fml)
       (set! extra-constraints (cons (r1cs:rassert (interp fml)) extra-constraints))]
      [(list 'constraint a b c)
       (set! constraints
             (cons (r1cs:constraint (r1cs:constraint-block (length a) (map second a) (map first+fix a))
                                    (r1cs:constraint-block (length b) (map second b) (map first+fix b))
                                    (r1cs:constraint-block (length c) (map second c) (map first+fix c)))
                   constraints))]))

  (set! constraints (reverse constraints))
  (set! extra-constraints (reverse extra-constraints))
  (set! input-list (reverse input-list))
  (set! output-list (reverse output-list))
  (define num-constraints (length constraints))

  (new (class* object% (r1cs-interface<%>)
         (super-new)

         (define/public (validate gen-witness)
           (when gen-witness
             (picus:user-error "sr1cs does not support witness generation")))

         (define/public (get-source)
           source)

         (define/public (get-format)
           "sr1cs")

         (define/public (get-num-wires)
           num-wires)

         (define/public (get-num-constraints)
           num-constraints)

         (define/public (get-prime-number)
           prime)

         (define/public (get-field-size)
           #f)

         (define/public (get-top-level-inputs)
           input-list)

         (define/public (get-top-level-outputs)
           output-list)

         (define/public (get-constraints)
           constraints)

         (define/public (get-extra-constraints)
           extra-constraints)

         (define/public (map-to-vars info)
           (for/list ([pair (in-list info)]
                      #:do [(match-define (cons k val) pair)])
             (cons (hash-ref r2c-map k k) val)))

         (define/public (gen-witness-files _raw-info)
           (picus:tool-error "sr1cs does not support witness generation")))))
