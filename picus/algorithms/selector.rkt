#lang racket

(provide counter
         first)

(define selector-interface<%>
  (interface ()
    apply-selector
    selector-feedback
    get-name))

; counter select strategy
; choose the signal that "contributes" the most to determine a uniqueness of
; other signals.
; i.e. the most "critical" one for propagation

(define counter
  (new (class* object% (selector-interface<%>)
         (super-new)
         (define signal-weights (make-hash))
         (define (signal-weights-ref k) (hash-ref signal-weights k 0))
         (define (signal-weights-dec! k v) (hash-update! signal-weights k (λ (old) (- old v)) 0))

         (define/public (apply-selector uspool weight-map)
           ; copy the counter and filter out non uspool ones
           (define tmp-counter (make-hash))
           (for ([(var counter) (in-hash weight-map)]
                 #:when (set-member? uspool var))
             (hash-set! tmp-counter var (+ counter (signal-weights-ref var))))
           ; add remaining uspool ones into the counter
           (for ([var uspool]
                 #:unless (hash-has-key? tmp-counter var))
             (hash-set! tmp-counter var 0))
           ; sort and pick
           (car (argmax cdr (hash->list tmp-counter))))

         ; adjust internal states according to the solver result
         (define/public (selector-feedback sid act)
           ; decrease the weight of the selected id since it's not solved
           (when (equal? 'skip act)
             (signal-weights-dec! sid 1)))

         (define/public (get-name) "counter"))))

; naive select strategy
; simply choose the first signal from the pool

(define first
  (new (class* object% (selector-interface<%>)
         (super-new)
         (define/public (apply-selector uspool cntx) (set-first uspool))
         (define/public (selector-feedback sid act) (void))
         (define/public (get-name) "first"))))
