#lang racket/base

(provide list-no-order-known-first)

(require racket/match
         racket/list
         (for-syntax racket/base
                     syntax/parse))

;; (list-no-order-known-first #:pred p? #:match x #:else y ...) is a match expander
;; that matches a list value. The first element that satisfies p? is further
;; matched against x, and other elements are matched against (list y ...).
;;
;; Examples:
;;
;; (match (list 2 1 4)
;;   [(list-no-order-known-first
;;     #:pred odd?
;;     #:match a
;;     #:else b ...)
;;    (list a b)])
;;
;; matches a with 1 and b ... with (2 4)

(define-match-expander list-no-order-known-first
  (syntax-parser
    [(_ #:pred p? #:match x #:else y ...)
     #`(? list?
          (app (let ([pred? p?])
                 (λ (xs)
                   (define-values (pos neg) (partition pred? xs))
                   (cond
                     [(null? pos) (values #f pos neg)]
                     [else (values #t (car pos) (append (cdr pos) neg))])))
               #t x (list y ...)))]))

(module+ test
  (require rackunit)
  (check-equal?
   (match (list 1 2 4)
     [(list-no-order-known-first
       #:pred odd?
       #:match (? (λ (x) (= x 1)) a)
       #:else b ...)
      (list a b)])
   '(1 (2 4)))

  (check-equal?
   (match (list 2 1 4)
     [(list-no-order-known-first
       #:pred odd?
       #:match a
       #:else b ...)
      (list a b)])
   '(1 (2 4)))

  (check-equal?
   (match (list (list 2 1))
     [(list (list-no-order-known-first
             #:pred odd?
             #:match a
             #:else b))
      (list a b)])
   '(1 2))

  (check-equal?
   (match (list (list 2 3 5) 1 (list 7 9 4))
     [(list-no-order-known-first
       #:pred (λ (x) (and (number? x) (odd? x)))
       #:match a
       #:else (list-no-order-known-first
               #:pred even?
               #:match b
               #:else c ...) ...)
      (list a b c)])
   '(1 (2 4) ((3 5) (7 9)))))
