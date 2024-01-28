#lang racket/base

(provide get-tmpdir
         clean-tmpdir!)
(require racket/file
         "logging.rkt")

(define tmpdir #f)

(define (get-tmpdir)
  (unless tmpdir
    (set! tmpdir (make-temporary-directory "picus~a"))
    (picus:log-info "working directory: ~a" tmpdir))
  tmpdir)

(define (clean-tmpdir!)
  (when tmpdir
    (delete-directory/files tmpdir)))
