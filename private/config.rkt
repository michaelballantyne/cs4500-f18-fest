#lang racket/base

(require racket/contract)
(provide
  CS4500-CONFIG-ID
  cs4500-default#
  ;;
  log-cs4500-f18-debug
  (rename-out [log-info/timestamp log-cs4500-f18-info])
  log-cs4500-f18-warning
  log-cs4500-f18-error
  log-cs4500-f18-fatal
  ;;
  harness-exe-path staff-exe-path staff-tests-path student-exe-name student-test-name
  student-root output-root assignment-name team-commits
  student-test-num
  ;;
  fest-config/c
  fest-config-key/c
  fest-config-value/c
  (contract-out
    [MAX-FILE-BYTES exact-nonnegative-integer?]
    [MF.txt string?]
    [AUDIT.txt string?]
    [cs4500-f18-logger
     logger?]
    [complete-path-to-file/c
      (-> any/c boolean?)]
    [complete-path-to-directory/c
      (-> any/c boolean?)]
    [cs4500-manifest-file?
      (-> path-string? boolean?)]
    [iso8601-string?
      (-> any/c boolean?)]
    [manifest->config
      (-> (and/c path-string? file-exists?) fest-config/c)]))

(require
  (only-in syntax/parse/define define-syntax-parse-rule)
  (only-in lang-file/read-lang-file lang-file-lang)
  (only-in gregor exn:gregor:parse? iso8601->date date? ~t now))

(module+ test
  (require rackunit))

;; =============================================================================

(define-logger cs4500-f18)

(define-syntax-parse-rule (log-info/timestamp fmt args ...)
  (let ([time-str (~t (now) "M/d:HH:mm:ss")])
    (log-cs4500-f18-info (string-append time-str " - " fmt) args ...)))

(define CS4500-CONFIG-ID 'cs4500-config)
(define cs4500-default#
  (make-immutable-hasheq '()))

(define MF.txt "MF.txt")
(define AUDIT.txt "audit.txt")
(define MAX-MB (expt 2 10))
(define MAX-FILE-BYTES (* 20 MAX-MB))

(define (manifest->config ps)
  (parameterize ([current-namespace (make-base-namespace)])
    (dynamic-require ps CS4500-CONFIG-ID)))

(define (iso8601-string? x)
  (and (string? x)
       (with-handlers ((exn:gregor:parse? (lambda (e) #false)))
         (date? (iso8601->date x)))))

(module+ test
  (test-case "iso8601-string?"
    (check-pred iso8601-string? "2018-10-02T01:00:00")))

(define-syntax-rule (define-symbol* x ...)
  (begin (define x 'x) ...))

(define-symbol*
  harness-exe-path staff-exe-path staff-tests-path student-exe-name student-test-name
  student-root output-root assignment-name team-commits
  student-test-num)

(define fest-config-key/c
  (or/c harness-exe-path
        staff-exe-path
        staff-tests-path
        student-exe-name
        student-test-name
        student-test-num
        student-root
        output-root
        assignment-name
        team-commits))

(define complete-path-to-file/c
  (and/c path-string? complete-path? file-exists?))

(define complete-path-to-directory/c
  (and/c path-string? complete-path? directory-exists?))

(define (fest-config-value/c k)
  (case k
    ((harness-exe-path)
     complete-path-to-file/c)
    ((staff-exe-path)
     complete-path-to-file/c)
    ((staff-tests-path)
     complete-path-to-directory/c)
    ((student-exe-name)
     (and/c path-string? relative-path?))
    ((student-test-name)
     (and/c path-string? relative-path?))
    ((student-test-num)
     exact-nonnegative-integer?)
    ((student-root)
     complete-path-to-directory/c)
    ((output-root)
     complete-path-to-directory/c)
    ((assignment-name)
     (or/c symbol? exact-nonnegative-integer?))
    ((team-commits)
     complete-path-to-file/c)))

(define fest-config/c
  (lambda (h)
    (for ((k (in-list (list harness-exe-path staff-exe-path staff-tests-path student-exe-name student-test-name student-root output-root assignment-name team-commits))))
      (unless (hash-has-key? h k)
        (raise-arguments-error 'fest-config/c "missing key" "key" k "hash" h))
      (define v (hash-ref h k))
      (unless ((fest-config-value/c k) v)
        (raise-arguments-error 'fest-config/c "bad value for key" "key" k "value" v "hash" h))
      (void)))
  #;(hash/dc
    [k fest-config-key/c]
    [v (k) (fest-config-value/c k)]
    #:immutable #true
    #:kind 'flat))

(define (cs4500-manifest-file? ps)
  (string=? (lang-file-lang ps)
            "cs4500-f18-fest/manifest"))
