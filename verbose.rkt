#lang racket/base

(provide vprintf
         set-verbose!)

(require racket/string
         racket/match)

(define verbose 0)
(define (set-verbose! v)
  (set! verbose v))

;; verbose-aware printf
;; when the level is 0, don't print anything
;; when the level is 1, print with ~e
;;   (which trims the output with ... when it's too long)
;; when the level is 2, print with ~a
(define (vprintf fmt . args)
  (match verbose
    [0 (void)]
    [1 (apply printf fmt args)]
    [2 (apply printf (string-replace fmt "~e" "~a") args)]))
