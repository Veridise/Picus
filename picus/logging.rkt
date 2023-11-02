;; This module creates a middleware for logging.
;; We want to reuse Racket's logging facility, since it already handles
;; difficult parts like logging in the presence of concurrency.
;; On the other hand, the logging facility is limited, and doesn't work well with
;; the protocol that we are targeting. So we need to create this bridge.
;;
;; In particular, the middleware introduces Picus logging level,
;; which does not need to agree with Racket logging level.
;; This is so that we are able to create custom levels, as Racket's logging has
;; fixed levels.
;;
;; The data field must always contain these keys:
;; - msg: the message to be logged for JSON logging
;; - level: the Picus's logging level
;; - caller: the source location of the log call.
;;     Racket does not have an information about the caller name,
;;     so the srcloc won't have the function name.
;;
;; The message field itself is used for text logging.
;; Usually, the message field and the msg key under the data field
;; would be identical, but they can also diverge if needed.

#lang racket/base


(provide define-log-function
         define-picus-level

         get-level
         get-levels

         picus-logger
         current-truncate?

         picus:log-debug
         picus:log-info
         picus:log-warning
         picus:log-error
         picus:log-critical

         picus:log-exception

         level:debug
         level:info
         level:warning
         level:error
         level:critical
         level:accounting
         level:progress

         ;; non-standard level
         picus:log-main
         picus:log-progress
         (rename-out [picus:log-accounting* picus:log-accounting])

         define/caller)

(require racket/hash
         racket/exn
         racket/string
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax
                     racket/path
                     racket/runtime-path)
         "ansi.rkt")

(begin-for-syntax
  (define-runtime-path picus-root ".."))

(define-logger picus)

(define current-truncate? (make-parameter #t))

(define (do-truncate fmt)
  (if (current-truncate?)
      fmt
      (string-replace fmt "~e" "~a")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging level definer

(define levels (make-hash))

(define (get-level level)
  (hash-ref levels level #f))

(define (get-levels)
  (map car (sort (hash->list levels) < #:key cdr)))

;; (define-picus-level picus-level level) defines a new Picus log level.
;; The level name is in upppercase
(define-syntax-parse-rule (define-picus-level picus-level:id level:integer)
  #:with level-str (string-upcase (symbol->string (syntax-e #'picus-level)))
  #:with level-id (format-id #'picus-level "level:~a" #'picus-level)
  (begin
    (define level-id 'level-str)
    (hash-set! levels level-id level)))

;; (define-log-function name [#:picus picus-level] [#:rkt rkt-level])
;; defines a new logging function `name`.
;; This logging function has the Picus logging level `picus-level`
;; and the Racket logging level `rkt-level`.
;; By default, both `picus-level` and `rkt-level` are defaulted to `name`.
;;
;; The macro binds `picus:log-<name>` to a logging function (actually a macro).
;; `picus:log-<name>` accepts a format string, followed by arguments to the format.
;; A combination of the format string and arguments are used for JSON logging.
;; The keyword argument #:extra can be used to pass an additional hash table
;; that contains extra information.
;; The keyword argument #:text-msg is similar to the format string,
;; and it is used for the text logging. By default, #:text-msg is defaulted to msg.
;;
;; The format string should use ~e for argument values that are truncatable
;; When current-truncate? is false, the full value is printed.
;; But when current-truncate? is true, the value is truncated.
(define-syntax-parse-rule (define-log-function name:id
                            {~optional {~seq #:picus picus-level:id}
                                       #:defaults ([picus-level #'name])}
                            {~optional {~seq #:rkt rkt-level:id}
                                       #:defaults ([rkt-level #'name])})
  #:fail-unless
  (member (syntax-e #'rkt-level) '(debug info warning error fatal none))
  "Racket's log level must be one of: debug, info, warning, error, fatal, and none"

  #:with level-id (format-id #'picus-level "level:~a" #'picus-level)

  #:with picus-log-id (format-id #'name "picus:log-~a" #'name)
  (define/caller (picus-log-id text-msg #:extra [extra (hash)] #:json-msg [json-msg text-msg] . args)
    #:caller caller
    (log-message picus-logger
                 'rkt-level
                 (logger-name picus-logger)
                 (apply format (do-truncate text-msg) args)
                 ;; prefer the user's extra over framework's information
                 ;; since we want the user to be able to override the information
                 (hash-union extra (hash 'caller caller
                                         'level level-id
                                         'msg (strip-ansi (apply format (do-truncate json-msg) args)))
                             #:combine (λ (left _right) left))
                 #f)))

(define-syntax-parse-rule (define/caller (name:id . args) #:caller caller:id body ...+)
  (begin
    (define internal-fun
      (procedure-rename (λ (#:caller caller . args) body ...) 'name))
    (define-syntax-parse-rule (name . inner-args)
      #:do [(define src (syntax-source this-syntax))
            (define line (syntax-line this-syntax))
            (define column (syntax-column this-syntax))]
      #:with caller-lit (format "~a:~a:~a"
                                (find-relative-path (simple-form-path picus-root) src)
                                line
                                column)
      (internal-fun #:caller 'caller-lit . inner-args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Built-in logging levels

(define-picus-level debug 10)
(define-picus-level info 20)
(define-picus-level warning 30)
(define-picus-level error 40)
(define-picus-level critical 50)

;; custom levels
;; note that in SaaS, accounting has level 25 and progress has level 26
;; but for standalone application, both levels are very noisy,
;; so we set them below the info level (20).
(define-picus-level accounting 15)
(define-picus-level progress 16)

;; Mostly from https://docs.python.org/3/library/logging.html#logging-levels
(define-log-function debug)
(define-log-function info)
(define-log-function warning)
(define-log-function error)
(define-log-function critical #:rkt fatal)

;; This is for the main output.
(define-log-function main #:picus info #:rkt info)
;; This is for the algorithm progress.
(define-log-function progress #:rkt debug)
;; Accounting
(define-log-function accounting #:rkt debug)

(define/caller (picus:log-accounting* #:type entry-type
                                      #:unit [entry-unit "unit"]
                                      #:value [entry-value 1]
                                      #:msg [msg ""])
  #:caller caller

  (picus:log-accounting "~a" msg
                        #:extra (hash 'entry_type entry-type
                                      'entry_unit entry-unit
                                      'entry_value entry-value
                                      'caller caller)))

(define/caller (picus:log-exception e) #:caller caller
  (picus:log-error "exception: ~e" (exn->string e) #:extra (hash 'caller caller)))
