#lang racket/base

(provide get-global-inputs)

(require racket/set
         racket/match
         csv-reading
         (prefix-in r1cs: "./r1cs/r1cs-grammar.rkt"))

(define (get-global-inputs r1cs-path sym-path)
  (define r0 (r1cs:read-r1cs r1cs-path))
  (define input-signals (r1cs:r1cs-inputs r0))
  (define ins (list->set input-signals))
  (define sym-table
    (with-input-from-file sym-path
      (λ () (csv->list (current-input-port)))))
  (define inputs
    (for/set ([row (in-list sym-table)]
              ;; Get the name after "main." without including array indexing
              #:do [(match-define (list id _ _ (pregexp #px"^main\\.(.*?)(?:\\[.*\\])?$" (list _ name _ ...))) row)]
              #:when (set-member? ins (string->number id)))
      name))
  (set->list inputs))

(module+ test
  (require rackunit)
  (check-equal? (get-global-inputs "../benchmarks/circomlib-cff5ab6/AliasCheck@aliascheck.r1cs"
                                   "../benchmarks/circomlib-cff5ab6/AliasCheck@aliascheck.sym")
                (set "in"))
  (check-equal? (get-global-inputs "../benchmarks/circomlib-cff5ab6/BabyAdd@babyjub.r1cs"
                                   "../benchmarks/circomlib-cff5ab6/BabyAdd@babyjub.sym")
                (set "x1" "y1" "x2" "y2")))
