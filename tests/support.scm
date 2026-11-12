;;; (tests support) - shared SRFI-64 helpers

(define-module (tests support)
  #:use-module (srfi srfi-64)
  #:export (test-finish repository-root))

(define (test-finish name)
  "End the test group NAME and exit non-zero if any test failed."
  (let ((runner (test-runner-current)))
    (test-end name)
    (exit (if (and (= 0 (test-runner-fail-count runner))
                   (= 0 (test-runner-xpass-count runner)))
              0
              1))))

(define (repository-root)
  (dirname (dirname (current-filename))))
