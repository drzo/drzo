(use-modules (srfi srfi-1) (srfi srfi-64) (ice-9 rdelim)
             (tests support) (ghost series) (ghost a000081))

(test-begin "a000081")

;;; OEIS b-file fixture.

(define (read-b-file file)
  (call-with-input-file file
    (lambda (port)
      (let loop ((acc '()))
        (let ((line (read-line port)))
          (cond ((eof-object? line) (reverse acc))
                ((or (string-null? (string-trim-both line))
                     (char=? #\# (string-ref (string-trim-both line) 0)))
                 (loop acc))
                (else
                 (let ((fields (string-tokenize line)))
                   (loop (cons (cons (string->number (car fields))
                                     (string->number (cadr fields)))
                               acc))))))))))

(define fixture
  (read-b-file (string-append (repository-root) "/tests/fixtures/b000081.txt")))

(test-equal "fixture has terms 0..30" (iota 31) (map car fixture))
(test-equal "first 31 terms match OEIS"
  (map cdr fixture)
  (map a000081 (map car fixture)))
(test-equal "a000081-list" (map cdr fixture) (a000081-list 31))
(test-equal "empty list" '() (a000081-list 0))

;;; The recurrence, evaluated directly from the definition.

(define (divisor-sum k)
  (fold (lambda (d sum) (if (= 0 (remainder k d)) (+ sum (* d (a000081 d))) sum))
        0
        (iota k 1)))

(test-assert "divisor sums match the definition"
  (every (lambda (k) (= (divisor-sum k) (a000081-divisor-sum k))) (iota 80 1)))

(test-assert "recurrence a(n+1) = (1/n) sum s(k) a(n-k+1)"
  (every (lambda (n)
           (= (* n (a000081 (+ n 1)))
              (fold (lambda (k sum)
                      (+ sum (* (divisor-sum k) (a000081 (+ (- n k) 1)))))
                    0
                    (iota n 1))))
         (iota 80 1)))

;;; The cache grows past any fixed size, in either order of access.

(test-assert "terms beyond index 1000" (> (a000081 1100) (a000081 1099)))
(test-equal "earlier terms are unchanged after the cache grows"
  (a000081 30)
  (cdr (last fixture)))
(test-error "negative index" #t (a000081 -1))
(test-error "inexact index" #t (a000081 2.0))

;;; A(x) = x exp(sum_{k>=1} A(x^k)/k), coefficient by coefficient.

(define precision 60)
(define A (a000081-series precision))

(test-equal "functional equation to x^60"
  A
  (series-shift
   (series-exp
    (fold (lambda (k sum)
            (series-add sum (series-scale (/ 1 k) (series-stretch A k))))
          (series-zero precision)
          (iota precision 1)))
   1))

;;; A(x) = x prod_{k>=1} (1 - x^k)^(-a(k)).

(test-equal "product formula to x^60"
  A
  (series-shift
   (fold (lambda (k prod)
           (series-mul prod (series-geometric-power precision k (a000081 k))))
         (series-one precision)
         (iota precision 1))
   1))

;;; Asymptotics.  Published values: alpha = 2.95576528565199497...,
;;; C = 0.43992401257102530... (OEIS A051491 and A187770).

(define (close? x y tolerance) (< (abs (- x y)) tolerance))

(test-assert "alpha" (close? (a000081-alpha) 2.955765285651995 1e-13))
(test-assert "rho" (close? (a000081-rho) 0.3383218568992077 1e-14))
(test-assert "Otter constant"
  (close? (a000081-otter-constant) 0.4399240125710253 1e-13))

(define ratios
  (map (lambda (n) (/ (a000081 n) (a000081-asymptotic n))) (iota 451 50)))

(test-assert "a(n) / asymptotic decreases monotonically for n in 50..500"
  (every > ratios (cdr ratios)))
(test-assert "a(n) / asymptotic stays above 1" (every (lambda (r) (> r 1.0)) ratios))
(test-assert "a(500) / asymptotic is within 3e-4 of 1"
  (close? (last ratios) 1.0 3e-4))
(test-assert "a(n+1)/a(n) approaches alpha"
  (close? (exact->inexact (/ (a000081 501) (a000081 500)))
          (a000081-alpha)
          1e-2))

(test-finish "a000081")
