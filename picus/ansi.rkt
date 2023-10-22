#lang racket

(provide highlight
         strip-ansi)

(define (ansi-code code)
  (format "~a~a" (integer->char #x1b) code))

(define (highlight s)
  (~a (ansi-code "[33m") s (ansi-code "[0m")))

(define (strip-ansi s)
  (regexp-replace*
   (string-append (regexp-quote (string (integer->char #x1b)))
                  "\\[[0-9;]*m")
   s
   ""))

(module+ test
  (require rackunit)
  (check-equal? (strip-ansi (highlight "abc")) "abc"))
