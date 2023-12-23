
#lang racket/base

(require racket/logging
         racket/match
         racket/hash
         racket/date
         json
         "logging.rkt"
         "exit.rkt")

(provide with-framework)

(define env-vars '(SERVICE_ID USER_ID CLIENT_ID TASK_ID BATCH_ID VERSION_ID PROJECT_ID CORRELATION_ID))
(define fetched-env-vars
  (for/hash ([var (in-list env-vars)]
             #:do [(define val (getenv (symbol->string var)))]
             #:when val)
    (values var val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; printing functions

(define (should-display? level)
  (>= (get-level level) (get-level (current-level))))

;; Everything above the warning level should be logged to stderr
(define (to-stderr? level)
  (>= (get-level level) (get-level level:warning)))

;; print-json has the following characteristics
;; (1) It completely disregards current-level
;;     (that is, it doesn't call should-display? at all)
;;     because there is already a service to filter JSON information
;; (2) It ignores the message field, since msg key already contains the message.
;; (3) It only uses current-outp.
(define (print-json #:level level
                    #:message _message
                    #:data data
                    #:timestamp timestamp)
  (define json-port (current-json-port))
  (when json-port
    (write-json (hash-union fetched-env-vars
                            data
                            (hash 'timestamp timestamp
                                  'level level
                                  'logger_name "picus")
                            #:combine (λ (left _right) left))
                json-port)
    (newline json-port)))

;; print-text has the following characeteristics
;; (1) It ignores the data field, since it is usually too verbose to
;;     print the data. This includes the msg key in the data field,
;;     which is for JSON logging.
;; (2) If (current-json-port) is the same as (current-output-port),
;;     we do not print anything.
(define (print-text #:level level
                    #:message message
                    #:data _data
                    #:timestamp timestamp)
  (unless (eq? (current-json-port) (current-output-port))
    (displayln message (if (to-stderr? level) (current-errp) (current-outp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameters

(define current-outp (make-parameter (current-output-port)))
(define current-errp (make-parameter (current-error-port)))
(define current-level (make-parameter 'info))
(define current-json-port (make-parameter #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameters

;; hash-pop :: hash? any/c #:default (or/c #f (-> any/c)) -> (values any/c hash?)
;; Pops the key. In case the key is not present, returns the default value when
;; default is a thunk. Otherwise, when default is #f (which is the default),
;; raises an error.
(define (hash-pop ht key #:default [default #f])
  (values (if default
              (hash-ref ht key default)
              (hash-ref ht key))
          (hash-remove ht key)))

(define (framework:core proc)
  (with-intercepted-logging
    (λ (l)
      (match-define (vector _ message data _) l)
      (define-values (level data*) (hash-pop data 'level))
      (define (do-print print-proc)
        (print-proc
         #:level level
         #:message message
         #:data data*
         #:timestamp (parameterize ([date-display-format 'iso-8601])
                       (date->string (current-date) #t))))
      (when (should-display? level)
        (do-print print-json)
        (do-print print-text)))
    (λ ()
      (with-handlers ([exn:fail? (λ (e)
                                   (picus:log-exception e)
                                   (picus:exit exit-code:tool-failure))])
        (proc)))
    #:logger picus-logger
    'debug ; debug is the lowest level of logging, so this intercepts everything
    #f))

;; with-framework :: #:out (or/c output-port? path-string?) = (current-output-port)
;;                   #:err (or/c output-port? path-string? 'out) = (current-error-port)
;;                   #:json-target (or/c #f string?)
;;                   #:truncate? boolean?
;;                   #:level string?
;;                   (-> never/c)
;;                   ->
;;                   never
;; Note that if #:err is the same as #:out, then 'out should be given.
;; so that we avoid (potentially) opening the same file twice.
;;
;; The proc positional argument must never return. Instead, it should exit with
;; picus:exit.
(define (with-framework
          #:truncate? truncate?
          #:level level
          #:json-target [json-target #f]
          #:out [out (current-output-port)]
          #:err [err (current-error-port)]
          proc)
  (let loop ([out out] [err err] [json-target json-target])
    (cond
      ;; configure out
      [(path-string? out)
       (call-with-output-file* out
         #:exists 'truncate
         (λ (outp) (loop outp err json-target)))]

      ;; configure err
      [(path-string? err)
       (call-with-output-file* err
         #:exists 'truncate
         (λ (errp) (loop out errp json-target)))]
      [(eq? 'out err) (loop out out json-target)]

      ;; configure json-target
      [(equal? "-" json-target) (loop out err (current-output-port))]
      [(path-string? json-target)
       (call-with-output-file* json-target
         #:exists 'truncate
         (λ (jsonp) (loop out err jsonp)))]

      ;; main
      [else
       (parameterize ([current-outp out]
                      [current-errp err]
                      [current-truncate? truncate?]
                      [current-json-port json-target]
                      [current-level level])
         (framework:core proc))])))
