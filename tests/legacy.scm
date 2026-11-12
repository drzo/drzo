;;; The original top-level scripts: loading one must print nothing and write
;;; nothing, and running its `main' must succeed and leave the committed
;;; TOML files byte-identical.

(use-modules (srfi srfi-1) (srfi srfi-64) (ice-9 popen) (ice-9 textual-ports)
             (tests support))

(test-begin "legacy")

(define root (repository-root))
(chdir root)

(define guile (or (getenv "GUILE") "guile"))

(define scripts
  '("a000081.scm" "a000081-toml.scm" "advanced-structures.scm"
    "enhanced-advanced-structures.scm" "ghost-in-guile.scm"
    "gpt2-hypergraph.scm" "simple-demo.scm" "unified-demo.scm"))

(define toml-files '("gpt2-a000081-config.toml" "neural-net-a000081.toml"))

(define (slurp file)
  (call-with-input-file file get-string-all))

(define (directory-listing)
  (let ((dir (opendir root)))
    (let loop ((acc '()))
      (let ((entry (readdir dir)))
        (if (eof-object? entry)
            (begin (closedir dir) (sort acc string<?))
            (loop (cons entry acc)))))))

(define (run . args)
  "Run guile with ARGS; return (exit-status . standard-output)."
  (let* ((port (apply open-pipe* OPEN_READ guile "--no-auto-compile" args))
         (output (get-string-all port))
         (status (close-pipe port)))
    (cons (status:exit-val status) output)))

(define toml-before (map slurp toml-files))
(define listing-before (directory-listing))

(for-each
 (lambda (script)
   (let ((result (run "-s" script)))
     (test-equal (string-append script ": load exits 0") 0 (car result))
     (test-equal (string-append script ": load prints nothing") "" (cdr result))))
 scripts)

(test-equal "loading creates no files" listing-before (directory-listing))
(test-equal "loading leaves the TOML files alone" toml-before (map slurp toml-files))

(for-each
 (lambda (script)
   (let ((result (run "-e" "main" "-s" script)))
     (test-equal (string-append script ": main exits 0") 0 (car result))
     (test-assert (string-append script ": main prints its demo")
       (not (string-null? (cdr result))))))
 scripts)

(test-equal "the demos regenerate the TOML files byte-identically"
  toml-before
  (map slurp toml-files))
(test-equal "the demos create no other files" listing-before (directory-listing))

(test-finish "legacy")
