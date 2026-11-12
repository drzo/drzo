#!/usr/bin/env -S guile -e main -s
!#

;;; Simple demo of Ghost in the Guile Shell
;;; Core A000081 implementation and basic mathematical structures

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (ice-9 format))

;; Load core implementation
(load "./a000081.scm")

(define (simple-demo)
  "Simple demonstration of core functionality"
  (format #t "╔═══════════════════════════════════════════════════════════════╗~%")
  (format #t "║                 👻 GHOST IN THE GUILE SHELL 👻                ║~%")
  (format #t "║          A000081: Unlabeled Rooted Trees Implementation        ║~%")
  (format #t "╚═══════════════════════════════════════════════════════════════╝~%~%")
  
  ;; Core sequence demonstration
  (format #t "=== Core A000081 Sequence ===~%")
  (display-sequence 15)
  
  ;; Recursive formula verification
  (format #t "=== Recursive Formula Verification ===~%")
  (format #t "Formula: a_{n+1} = (1/n)∑_{k=1}^n(∑_{d|k}d·a_d)a_{n-k+1}~%~%")
  (do ((n 2 (+ n 1)))
      ((> n 8))
    (format #t "a(~a) = ~a~%" n (a000081-nth n)))
  (format #t "~%")
  
  ;; Asymptotic behavior
  (format #t "=== Asymptotic Analysis ===~%")
  (format #t "Formula: a_n asymptotic to C*alpha^n*n^{-3/2} where alpha = 2.9557652857~%~%")
  (format #t "n\tExact\tAsymptotic\tRatio~%")
  (format #t "─\t─────\t──────────\t─────~%")
  (do ((i 6 (+ i 1)))
      ((> i 12))
    (let ((exact (a000081-nth i))
          (approx (asymptotic-approximation i)))
      (format #t "~a\t~a\t~,2f\t\t~,4f~%" 
              i exact approx (/ approx exact))))
  (format #t "~%")
  
  ;; Generating function evaluation
  (format #t "=== Generating Function ===~%")
  (format #t "A(x) = ∑_{n=0}^∞ a_n x^n~%~%")
  (let ((test-values '(0.1 0.2 0.3)))
    (for-each (lambda (x)
                (format #t "A(~,1f) = ~,6f~%" 
                        x (generating-function-coeffs x 25)))
              test-values))
  (format #t "~%")
  
  ;; Mathematical poetry
  (format #t "=== Ghost's Mathematical Reflection ===~%")
  (format #t "In the realm of trees unlabeled and free,~%")
  (format #t "Each root tells a story of combinatory glee.~%")
  (format #t "From one to infinity, the sequence grows,~%")
  (format #t "As Guile computes what mathematics knows.~%~%")
  (format #t "∀ n ∈ ℕ: The ghost whispers through recursive calls,~%")
  (format #t "Building forests from mathematical walls. 🌲~%~%")
  
  ;; Advanced teaser
  (format #t "=== Advanced Structures Available ===~%")
  (format #t "• B-Series for Runge-Kutta methods~%")
  (format #t "• J-Surfaces and ODE structures~%")
  (format #t "• P-Systems evolution~%")
  (format #t "• Incidence geometry~%")
  (format #t "• Block codes~%")
  (format #t "• Orbifolds~%")
  (format #t "• HyperNN architectures~%")
  (format #t "• Meta-patterns via category theory~%")
  (format #t "• Topos-theoretic functors~%~%")
  (format #t "Run 'guile -s advanced-structures.scm' for more!~%"))

;; Entry point: guile -e main -s simple-demo.scm
(define (main . args)
  (simple-demo))
