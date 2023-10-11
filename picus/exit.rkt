#lang racket/base

(provide picus:exit
         exit-code:safe
         exit-code:unsafe
         exit-code:unknown
         exit-code:tool-failure
         exit-code:tool-error
         exit-code:user-error

         picus:user-error
         picus:tool-error)

(require "logging.rkt")

;; HACK: sleep allows Racket to flush log output before exiting
;; See https://github.com/racket/racket/issues/4773
(define (picus:exit code)
  (picus:log-info "Exiting Picus with the code ~a" code)
  (sleep 0.1)
  (exit code))

;; exits with the unknown status
(define exit-code:unknown 0)

;; exits with a guarantee
(define exit-code:safe 8)

;; exits with issues
(define exit-code:unsafe 9)

;; exits with tool failure (e.g., uncaught exception)
(define exit-code:tool-failure 1)

;; exits with tool error (e.g., internal error, or unreachable path reached)
(define exit-code:tool-error 10)

;; exits with user error (e.g., bad inputs)
(define exit-code:user-error 50)

(define/caller (picus:user-error fmt . args) #:caller caller
  (picus:log-error (apply format fmt args)
                   #:extra (hash 'caller caller))
  (picus:exit exit-code:user-error))

(define/caller (picus:tool-error fmt . args) #:caller caller
  (picus:log-error (apply format fmt args)
                   #:extra (hash 'caller caller))
  (picus:exit exit-code:tool-error))
