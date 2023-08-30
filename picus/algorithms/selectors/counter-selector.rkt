#lang racket
(provide apply-selector
         selector-init
         selector-feedback)

; shared stateful variables and methods
; signal weights
(define signal-weights null)
(define (signal-weights-reset!) (set! signal-weights (make-hash)))
(define (signal-weights-set! k v) (hash-set! signal-weights k v))
(define (signal-weights-ref k) (hash-ref signal-weights k))
(define (signal-weights-inc! k v) (hash-set! signal-weights k (+ (hash-ref signal-weights k) v)))
(define (signal-weights-dec! k v) (hash-set! signal-weights k (- (hash-ref signal-weights k) v)))

; =======================
; counter select strategy
; choose the signal that "contributes" the most to determine a uniqueness of
; other signals.
; i.e. the most "critical" one for propagation
(define (apply-selector uspool weight-map)
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

(define (selector-init nwires)
  (signal-weights-reset!)
  (for ([key (range nwires)])
    (signal-weights-set! key 0)))

; adjust internal states according to the solver result
(define (selector-feedback sid act)
  (cond
    ; decrease the weight of the selected id since it's not solved
    [(equal? 'skip act) (signal-weights-dec! sid 1)]
    ; otherwise do nothing
    [else (void)]))
