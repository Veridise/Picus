#lang racket/base

(provide get-tmpdir
         clean-tmpdir!)
(require racket/file)

(define tmpdir #f)

(define (get-tmpdir)
  (unless tmpdir
    (set! tmpdir (make-temporary-directory "picus~a")))
  tmpdir)

(define (clean-tmpdir!)
  (when tmpdir
    (delete-directory/files tmpdir)))
