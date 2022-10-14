#lang racket/base

;; Usage:
;;   racket main.rkt <FILE.cs4500>
;;
;; Reads the configuration data in <FILE.cs4500> and starts grading in the
;;  current filesystem.
;; - runs student executables on the staff tests
;; - validates student tests against the staff executable
;; - runs valid student exes against valid student tests in a test fest

(require racket/contract)
(provide
  )

(require
  file/glob
  (only-in racket/sandbox call-with-limits call-with-deep-time-limit exn:fail:resource?)
  (only-in racket/port call-with-output-string)
  (only-in racket/path file-name-from-path path-only)
  (only-in racket/file delete-directory/files)
  (only-in racket/string string-trim string-split)
  (only-in racket/system system*)
  (only-in racket/format ~a)
  (only-in racket/set for/set for*/set set-subtract set-empty? set->list)
  cs4500-f18-fest/private/config)

(module+ test
  (require rackunit))

;; =============================================================================

(define (testfest cfg)
  (define assn-name (hash-ref cfg assignment-name))
  (log-cs4500-f18-info "initializing fest: '~a'" assn-name)
  (define results-dir (init-fest cfg))
  (define r-str (path-string->string results-dir))
  (void
    (log-cs4500-f18-info "running student executable ('~a')  vs. staff tests ('~a')" (hash-ref cfg student-exe-name) (hash-ref cfg staff-tests-path))
    (student-exe-vs-staff-test results-dir cfg)
    #;(ask-for-help "finished checking student exe vs. staff tests --- does everything look okay in '~a'?~nPress enter to continue." r-str)
    (log-cs4500-f18-info "running staff executable ('~a') vs. student tests ('~a')" (hash-ref cfg staff-exe-path) (hash-ref cfg student-test-name))
    (staff-exe-vs-student-test results-dir cfg)
    #;(ask-for-help "finished checking staff exe vs. student tests --- does everything look okay in '~a'?~nPress enter to continue." r-str)
    (log-cs4500-f18-info "running TESTFEST for '~a'" (path-string->string results-dir))
    (fest-matrix results-dir cfg))
  (log-cs4500-f18-info "testfest complete, results in: '~a'" (path-string->string results-dir))
  (void))

(define (init-fest cfg)
  (define results-dir (config->results-dir cfg))
  (void
    (ensure-dir results-dir)
    (log-cs4500-f18-info "git checkout '~a'" (hash-ref cfg student-root))
    (git-checkout* cfg))
  results-dir)

(define (git-checkout* cfg)
  (define gits-dir (hash-ref cfg student-root))
  (define assn-name (hash-ref cfg assignment-name))
  (define deadline (hash-ref cfg student-deadline))
  (parameterize ([current-directory gits-dir])
    (for ((team-sym (in-list (hash-ref cfg team-name*))))
      (define team (symbol->string team-sym))
      (ensure-git-dir team)
      (git-checkout team assn-name deadline))))

(define (ensure-git-dir team)
  (define full-path (build-path (current-directory) team))
  (let loop ()
    (cond
      [(not (directory-exists? full-path))
       (ask-for-help "Directory does not exist: '~a'~nClone the Git repo and press enter to continue." (path-string->string full-path))
       (loop)]
      [(not (git-directory? full-path))
       (ask-for-help "Directory is not a git repo '~a'~nClone the Git repo and press enter to continue." (path-string->string full-path))
       (loop)]
      [else
       (void)])))

(define (git-directory? ps)
  (parameterize ((current-directory ps))
    (define-values [_ success?] (shell "git" '("rev-parse" "--is-inside-work-tree")))
    success?))

(define (git-checkout team assn-name deadline)
  (log-cs4500-f18-info "git checkout '~a'" team)
  (parameterize ((current-directory (build-path (current-directory) team)))
    (define tag-name (symbol->string assn-name))
    (shell/ask-for-help "git" (list "checkout" tag-name))
    #;(if (git-branch-exists? branch-name)
      (shell/ask-for-help "git" (list "checkout"branch-name))
      (let ()
        (shell/ask-for-help "git" '("checkout" "master"))
        (shell/ask-for-help "git" '("pull" "origin" "master"))
        (define pre-deadline-commit
          (let ([time-str (format "--before='~a'" deadline)])
            (shell/dontstop "git" (list "rev-list" "--date=iso" "--reverse" "-n" "1" time-str "master"))))
        (shell/ask-for-help "git" (list "checkout" pre-deadline-commit))
        (shell/ask-for-help "git" (list "checkout" "-b" branch-name))))
    (void)))

(define (git-branch-exists? branch-name)
  (define-values [_ success?] (shell "git" (list "rev-parse" "--verify" branch-name)))
  success?)

(define (shell pre-exe pre-cmd)
  (define exe (find-exe pre-exe))
  (define success? (box #f))
  (define cmd* (map path-string->string (if (path-string? pre-cmd) (list pre-cmd) pre-cmd)))
  (define str
    (call-with-output-string
      (lambda (p)
        (parameterize ((current-output-port p)
                       (current-error-port p))
          (set-box! success? (apply system* exe cmd*))))))
  (values (string-trim str) (unbox success?)))

(define (shell/ask-for-help pre-exe pre-cmd [help "Press enter to continue"])
  (define-values [str success?] (shell pre-exe pre-cmd))
  (if success?
    str
    (let ()
      (log-cs4500-f18-error "~a" str)
      (log-cs4500-f18-info "current directory: ~a" (path-string->string (current-directory)))
      (ask-for-help help))))

(define (shell/dontstop pre-exe pre-cmd)
  (define-values [str _] (shell pre-exe pre-cmd))
  str)

;; find-exe : path-string? -> path-string?
(define (find-exe pre-exe)
  (define fep (find-executable-path pre-exe))
  (if (path? fep)
    fep
    (raise-user-error 'shell "cannot find executable '~a', please install and try again" pre-exe)))

(define (ask-for-help str . arg*)
  (interactive-prompt (if (null? arg*) str (apply format str arg*)) void))

(define (interactive-prompt str ok?)
  (let loop ()
    (displayln str)
    (define v (read-line))
    (or (ok? v)
        (loop))))

(define (ensure-dir dir)
  (unless (directory-exists? dir)
    (make-directory dir)))

(define (config->results-dir cfg)
  (define base (hash-ref cfg student-root))
  (define name (symbol->string (hash-ref cfg assignment-name)))
  (build-path base name))

(module+ test
  (test-case "config->results-dir"
    (check-equal? (config->results-dir
                    (make-immutable-hash `((,student-root . "A/B")
                                           (,assignment-name . C))))
                  (build-path "A/B" "C"))))

(define (student-exe-vs-staff-test results-dir cfg)
  ;; for every team name, if haven't run staff tests yet,
  ;;  check that exe exists + is executable,
  ;;  run MF tests, save to file
  ;;  stop and print the process list in-between
  (define s-root (hash-ref cfg student-root))
  (define s-exe-name (hash-ref cfg student-exe-name))
  (define staff-tests (hash-ref cfg staff-tests-path))
  (define *first-time (box #true))
  (define exe-time-limit (or (hash-ref cfg max-seconds) MAX-EXE-SECONDS))
  (define assn-name-str (symbol->string (hash-ref cfg assignment-name)))
  (for ((this-name-sym (in-list (hash-ref cfg team-name*))))
    (define this-name-str (symbol->string this-name-sym))
    (define team-r-dir (build-path results-dir this-name-str))
    (void (ensure-dir team-r-dir))
    (define team-make-file (build-path s-root this-name-str assn-name-str "makefile"))
    (define team-Make-file (build-path s-root this-name-str assn-name-str "Makefile"))
    (define prev-make.txt (build-path s-root this-name-str assn-name-str "make.txt"))
    (define make-path (build-path team-r-dir "make.txt"))
    (when (file-exists? prev-make.txt)
      (log-cs4500-f18-info "Moving vet make results for ~a" this-name-str)
      (copy-file prev-make.txt (build-path team-r-dir "first-make.txt") #t)
      (delete-file prev-make.txt))
    (define already-made? (file-exists? make-path))
    (when (and (not already-made?)
               (or (file-exists? team-make-file) (file-exists? team-Make-file)))
      (log-cs4500-f18-warning "about to run student Makefile, current ps -f:~n~a" (current-process-list))
      (define custodian (make-custodian))
      (parameterize ((current-custodian custodian)
                     (current-subprocess-custodian-mode 'kill)
                     (subprocess-group-enabled
                      (or (eq? (system-type) 'unix) (eq? (system-type) 'macosx))))
          (parameterize ((current-directory (path-only team-make-file)))
            (define MAKE-TIMEOUT (* 20 60))
            (define m-str
              (with-handlers ([exn:fail:resource? (lambda (exn) "Took longer than ~a seconds" MAKE-TIMEOUT)])
                (call-with-limits MAKE-TIMEOUT #f
                                  (lambda () (shell/dontstop "make" (list))))))
            (with-output-to-file (build-path team-r-dir "make.txt") (lambda () (displayln m-str)))
            (custodian-shutdown-all custodian))))
    (define team-mf (build-path team-r-dir MF.txt))
    (unless (file-exists? team-mf)
      (when (unbox *first-time)
        (set-box! *first-time #false)
        (log-cs4500-f18-warning "about to test student executables, current ps -f:~n~a" (current-process-list)))
      (define (write-team-output str)
        (with-output-to-file team-mf (lambda () (displayln str))))
      (define team-exe (build-path s-root this-name-str s-exe-name))
      (cond
        [(not (file-exists? team-exe))
         (write-team-output (format "file '~a' does not exist" (path->string team-exe)))]
        [(not (file-executable? team-exe))
         (write-team-output (format "file '~a' is not executable" (path->string team-exe)))]
        [else
         (log-cs4500-f18-warning "executing ~a" (path->string team-exe))
         (define r-str
           (parameterize ((current-directory (path-only team-exe)))
             (call-with-cs4500-limits exe-time-limit MAX-MB
               (lambda () (run-staff-harness cfg #:exe team-exe #:tests staff-tests)))))
         (write-team-output r-str)
         (log-cs4500-f18-warning "finished executing for '~a', current ps -f:~n~a~noutput:~a"
                                 (path->string team-exe)
                                 (current-process-list)
                                 r-str)
         #;(ask-for-help "Press enter to continue")
         (void)])))
  (void))

(define (staff-exe-vs-student-test results-dir cfg)
  ;; for every team name,
  ;;  find their tests if any,
  ;;  run the staff exe on each test ONE BY ONE, check for "PASSED"
  ;;  concat the outputs into one file,
  ;;  save valid tests into results dir
  (define name* (hash-ref cfg team-name*))
  (define staff-exe (hash-ref cfg staff-exe-path))
  (define test-path (hash-ref cfg student-test-name))
  (define test-name (path-string->string (file-name-from-path test-path)))
  (define s-root (hash-ref cfg student-root))
  (define test-time-limit (or (hash-ref cfg max-seconds) MAX-TEST-SECONDS))
  (log-cs4500-f18-info "test-time-limit: ~a" test-time-limit)
  (for ((this-name-sym (in-list name*)))
    (define this-name-str (symbol->string this-name-sym))
    (define this-r (build-path results-dir this-name-str))
    (define this-r-test (build-path this-r test-name))
    (unless (directory-exists? this-r-test)
      (make-directory this-r-test)
      (define this-tests (build-path s-root this-name-str test-path))
      (ensure-dir this-tests)
      (define expected-files (for*/set ([i (in-range MAX-NUM-TESTS)]
                                        [in? (in-list '(#t #f))])
                               (if in? (i-in.json i) (i-out.json i))))
      (define actual-in-dir (for/set ([p (in-list (directory-list this-tests))]) (path->string p)))
      (unless (equal? expected-files actual-in-dir)
        (log-cs4500-f18-info "Extra/Missing test files for ~a" this-name-str)
        (with-output-to-file (build-path this-r AUDIT.txt) #:exists 'append
          (lambda () (printf "Extra or missing files in tests directory\n"))))
      (for* ([i (in-range MAX-NUM-TESTS)]
             [test.in (in-value (build-path this-tests (format "~a-in.json" i)))]
             #:when (let ((out.json (in.json->out.json test.in)))
                      (and (file-exists? test.in)
                           (file-exists? out.json))))
        (log-cs4500-f18-info "auditing test '~a'" (path-string->string test.in))
        (define test.out (in.json->out.json test.in))
        (if (file-too-large? test.out)
          (with-output-to-file (build-path this-r AUDIT.txt) #:exists 'append
            (lambda () (printf "file ~s is too large (~a bytes)~n" (path-string->string test.in) (file-size test.in))))
          (let ()
            #;(log-cs4500-f18-info "Starting search for tmp-dir")
            (define tmp-dir
              (let loop ((acc this-tests))
                #;(log-cs4500-f18-info "checking ~a" acc)
                (if (directory-exists? acc)
                  (loop (path-add-extension acc (format "-~a" i)))
                  acc)))
            #;(log-cs4500-f18-info "Found temp dir ~a" tmp-dir)
            (void
              (make-directory tmp-dir)
              (copy-file test.in (build-path tmp-dir (file-name-from-path test.in)))
              (copy-file test.out (build-path tmp-dir (file-name-from-path test.out))))
            (define r-str
              (parameterize ((current-directory (path-only staff-exe)))
                (log-cs4500-f18-info "invoking staff exe on ~a" test.in)
                (call-with-cs4500-limits 20 #;test-time-limit 100
                  (lambda () (run-staff-harness cfg #:exe staff-exe #:tests tmp-dir)))))
            (cond
              [(student-test-passed? r-str)
               (log-cs4500-f18-info "good test! '~a'" (path-string->string test.in))
               (copy-file test.in (build-path this-r-test (file-name-from-path test.in)))
               (copy-file test.out (build-path this-r-test (file-name-from-path test.out)))]
              [else
               (log-cs4500-f18-info "bad test! '~a'" (path-string->string test.in))])
            (with-output-to-file (build-path this-r AUDIT.txt) #:exists 'append
              (lambda () (displayln r-str)))
            (delete-directory/files tmp-dir)
            (void))))))
  (void))

(define (file-too-large? ps)
  (< MAX-FILE-BYTES (file-size ps)))

;; `xtest` and the likes impose a time limit
;; limiting *this* racket process's memory doesn't accomplish anything
;; so just call the function
(define (call-with-cs4500-limits max-seconds max-mb f)
  (f))

(define ((cs4500-resource-handler max-seconds max-mb) ex)
  (format "~a~n time limit: ~a seconds~n" (exn-message ex) max-seconds))

(define (in.json->out.json ps)
  (define-values [base name _mbd?] (split-path ps))
  (define m (regexp-match #rx"^([^-]*)-in.json$" (path->string name)))
  (if m
    (let ([name.out (format "~a-out.json" (cadr m))])
      (if (path-string? base)
        (build-path base name.out)
        name.out))
    (raise-argument-error 'in.json->out.json "(stringof X-in.json)" ps)))

(module+ test
  (test-case "in.json->out.json"
    (check-equal? (path-string->string (in.json->out.json "1-in.json")) "1-out.json")
    (check-equal? (path-string->string (in.json->out.json "foo-in.json")) "foo-out.json")
    (check-equal? (path-string->string (in.json->out.json "/baz/bar/foo-in.json")) "/baz/bar/foo-out.json")))

(define (i-in.json i) (format "~a-in.json" i))
(define (i-out.json i) (format "~a-out.json" i))

(define (glob/debug pat)
  (define r* (glob pat))
  (log-cs4500-f18-info "glob pattern ~s ===> ~s" pat r*)
  r*)

(define (student-test-passed? str)
  (regexp-match? #rx"passed 1" str))

(define (fest-matrix results-dir cfg)
  ;; for every team with a "valid" executable,
  ;;  for every OTHER team with valid tests,
  ;;   run team-exe on OTHER-team-tests,
  ;;   save results to a file
  ;; (sandboxing would be nice, but its not essential)
  (define name* (hash-ref cfg team-name*))
  (define s-root (hash-ref cfg student-root))
  (define s-path (hash-ref cfg student-exe-name))
  (define exe-time-limit (or (hash-ref cfg max-seconds) MAX-EXE-SECONDS))
  (for ((this-name-sym (in-list name*))
        #:when (has-valid-testfest-exe? this-name-sym results-dir cfg))
    (log-cs4500-f18-info "testfest '~a' vs ..." this-name-sym)
    (log-cs4500-f18-warning "Current ps -f:~n~a" (current-process-list))
    (define this-name-str (~a this-name-sym))
    (define this-exe (build-path s-root this-name-str s-path))
    (define student-r-dir (build-path results-dir this-name-str))
    (for ((that-name-sym (in-list name*))
          #:when (and (not (eq? this-name-sym that-name-sym))
                      (let ((that-tests (make-tests-dir that-name-sym results-dir cfg)))
                        (non-empty-directory-exists? that-tests))))
      (define that.txt (build-path student-r-dir (format "~a.txt" that-name-sym)))
      (define that-tests (make-tests-dir that-name-sym results-dir cfg))
      (unless (file-exists? that.txt)
        (log-cs4500-f18-info "testfest '~a' vs '~a'" this-name-sym that-name-sym)
        (log-cs4500-f18-info "running xtest ~s ~s" (path-string->string this-exe) (path-string->string that-tests))
        (parameterize ((current-directory (path-only this-exe)))
          (with-output-to-file that.txt
            (lambda ()
              (displayln
                (call-with-cs4500-limits 20 #;exe-time-limit #;MAX-MB 100
                  (lambda () (run-staff-harness cfg #:exe this-exe #:tests that-tests))))))))))
  (void))

(define (current-process-list)
  (shell/dontstop "ps" "-f"))

(define (run-staff-harness cfg #:exe exe-path #:tests tests-path)
  (define h-exe (hash-ref cfg harness-exe-path))
  (define cc (make-custodian))
  (begin0
    (parameterize ((current-custodian cc)
                   (current-subprocess-custodian-mode 'kill))
        (shell/dontstop h-exe (list tests-path exe-path)))
    (custodian-shutdown-all cc)))

(define (make-tests-dir team-name results-dir cfg)
  (build-path results-dir
              (~a team-name)
              (file-name-from-path (hash-ref cfg student-test-name))))

(define (non-empty-directory-exists? ps)
  (and (path-string? ps)
       (directory-exists? ps)
       (not (null? (directory-list ps)))))

(define (has-valid-testfest-exe? team-name results-dir cfg)
  (define exe-path (build-path (hash-ref cfg student-root) (~a team-name) (hash-ref cfg student-exe-name)))
  (and (file-exists? exe-path)
       (file-executable? exe-path)))

(define (file-executable? ps)
  (memq 'execute (file-or-directory-permissions ps)))

(define (path-string->string x)
  (if (path? x) (path->string x) x))

;; =============================================================================

(module+ main
  (require racket/cmdline (only-in racket/file file->value))
  (define this-name 'cs4500-f18-fest)
  (command-line
    #:program (symbol->string this-name)
    #:args (manifest)
    (define cfg
      (if (cs4500-manifest-file? manifest)
        (manifest->config manifest)
        (contract fest-config/c (and (file-exists? manifest) (file->value manifest)) manifest this-name)))
    (log-cs4500-f18-info "configuration ~a" cfg)
    (testfest cfg)))


