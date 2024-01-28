#lang racket/base

(provide gen-witness)

(require racket/match
         racket/class
         "logging.rkt"
         "exit.rkt")

(define magic #"wtns")

(define (write-number n fs)
  (for/fold ([n n]) ([i (in-range fs)])
    (define-values (q r) (quotient/remainder n 256))
    (write-bytes (integer->integer-bytes r 1 #f))
    q))

(define (gen-witness raw-info r0)
  (picus:log-progress "[wtns] generating wtns files")
  (match-define (list in out1 out2 other1 other2) raw-info)
  (define (get-model out other)
    (cons 1 (map cdr (sort (append in out other) < #:key car))))
  (define m1 (get-model out1 other1))
  (define m2 (get-model out2 other2))
  (define fs (send r0 get-field-size))
  (define (do-gen m name)
    (unless (= (send r0 get-num-wires) (length m))
      (picus:tool-error "witness list incomplete: ~a" m))
    (picus:log-info "writing a wtns file ~a" name)
    (with-output-to-file name
      #:exists 'truncate
      (λ ()
        ;;;; wtns file
        ;; magic
        (write-bytes magic)
        ;; version (4 bytes, = 2)
        (write-bytes (integer->integer-bytes 2 4 #f))
        ;; number of sections (4 bytes, = 2 --- header section and witness section)
        (write-bytes (integer->integer-bytes 2 4 #f))

        ;;;; header
        ;; section type (4 bytes, = 1)
        (write-bytes (integer->integer-bytes 1 4 #f))
        ;; section size (8 bytes, = 4 + field-size + 4)
        (write-bytes (integer->integer-bytes (+ 4 fs 4) 8 #f))
        ;; field size (4 bytes)
        (write-bytes (integer->integer-bytes fs 4 #f))
        ;; prime number (fs bytes)
        (write-number (send r0 get-prime-number) fs)
        ;; witness length (4 bytes)
        (write-bytes (integer->integer-bytes (length m1) 4 #f))

        ;; witness
        ;; section type (4 bytes, = 2)
        (write-bytes (integer->integer-bytes 2 4 #f))
        ;; section size (8 bytes, = field-size * witness-length)
        (write-bytes (integer->integer-bytes (* (length m) fs) 8 #f))
        (for ([e (in-list m)])
          ;; signal (fs bytes)
          (write-number e fs)))))
  (do-gen m1 "first-witness.wtns")
  (do-gen m2 "second-witness.wtns"))
