#lang racket/base

(require racket/list
         syntax/parse/define
         rackunit
         json
         "../picus/framework.rkt"
         "../picus/exit.rkt"
         "../picus/logging.rkt")

(define-syntax-parse-rule (run-test
                           #:setup
                           setup-body ...+
                           #:check-status code
                           check-status-body ...+
                           #:check out:id err:id
                           check-body ...+)

  (let ([out-id (open-output-string)]
        [err-id (open-output-string)])
    (parameterize ([current-output-port out-id]
                   [current-error-port err-id])
      (let/cc return
        (parameterize ([exit-handler (λ (code)
                                       check-status-body ...
                                       (return #f))])
          setup-body ...)))
    (let ([out (get-output-string out-id)]
          [err (get-output-string err-id)])
      check-body ...)))

(test-case "test log level in text mode (partial log)"
  (run-test
   #:setup
   (with-framework
     #:json? #f
     #:truncate? #f
     #:level "ERROR"
     (λ ()
       (picus:log-error "foo")
       (picus:log-info "bar")))

   #:check-status code
   (check-equal? code 0)

   #:check out err
   (check-equal? out "")
   (check-regexp-match #px"foo" err)))

(test-case "test log level in text mode (full log)"
  (run-test
   #:setup
   (with-framework
     #:json? #f
     #:truncate? #f
     #:level "INFO"
     (λ ()
       (picus:log-error "foo")
       (picus:log-info "bar")))

   #:check-status code
   (check-equal? code 0)

   #:check out err
   (check-regexp-match #px"bar" out)
   (check-regexp-match #px"foo" err)))

(test-case "test log level in json mode"
  (run-test
   #:setup
   (with-framework
     #:json? #t
     #:truncate? #f
     #:level #f
     (λ ()
       (picus:log-error "foo")
       (picus:log-info "bar")))

   #:check-status code
   (check-equal? code 0)

   #:check out err
   (check-equal? err "")

   (define jsons (for/list ([json (in-port read-json (open-input-string out))]) json))

   ;; log-error
   (define json-err (first jsons))
   (check-regexp-match #px"framework-test\\.rkt:76:7" (hash-ref json-err 'caller))
   (check-equal? (hash-ref json-err 'level) "ERROR")
   (check-equal? (hash-ref json-err 'msg) "foo")

   ;; log-info
   (define json-info (second jsons))
   (check-regexp-match #px"framework-test\\.rkt:77:7" (hash-ref json-info 'caller))
   (check-equal? (hash-ref json-info 'level) "INFO")
   (check-equal? (hash-ref json-info 'msg) "bar")

   ;; exit-info
   (define json-exit (third jsons))
   (check-regexp-match #px"exit\\.rkt:18:2" (hash-ref json-exit 'caller))
   (check-equal? (hash-ref json-exit 'level) "INFO")
   (check-equal? (hash-ref json-exit 'msg) "Exiting Picus with the code 0")))

(test-case "test exit code"
  (run-test
   #:setup
   (with-framework
     #:json? #f
     #:truncate? #f
     #:level "INFO"
     (λ ()
       (picus:log-error "foo")
       (picus:tool-error "bad")
       (picus:log-info "bar")))

   #:check-status code
   (check-equal? code exit-code:tool-error)

   #:check out err
   ;; control not reached log-info
   (check-false (regexp-match #px"bar" out))
   (check-regexp-match #px"foo" err)))
