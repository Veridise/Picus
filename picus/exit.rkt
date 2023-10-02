#lang racket/base

(provide picus:exit
         exit-code:success
         exit-code:issues
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

;; exits normally
(define exit-code:success 0)

;; exits with issues
(define exit-code:issues 9)

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
