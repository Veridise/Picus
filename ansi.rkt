#lang racket

(provide highlight)

(define (ansi-code code)
  (format "~a~a" (integer->char #x1b) code))

(define (highlight s)
  (~a (ansi-code "[33m") s (ansi-code "[0m")))
