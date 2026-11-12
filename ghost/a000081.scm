;;; (ghost a000081) - OEIS A000081, the number of unlabelled rooted trees
;;;
;;;   a(0) = 0, a(1) = 1,
;;;   a(n+1) = (1/n) * sum_{k=1..n} s(k) * a(n-k+1),  s(k) = sum_{d|k} d*a(d)
;;;
;;; All sequence arithmetic is exact.  The caches grow on demand, and s(k)
;;; is stored, so computing a(0..n) takes O(n^2) multiplications.

(define-module (ghost a000081)
  #:use-module (ghost series)
  #:export (a000081
            a000081-list
            a000081-divisor-sum
            a000081-series
            a000081-rho
            a000081-alpha
            a000081-otter-constant
            a000081-asymptotic))

;; a-cache[i] = a(i) and s-cache[i] = s(i) for 0 <= i <= known.
(define a-cache (vector 0 1))
(define s-cache (vector 0 1))
(define known 1)

(define (grow v size)
  (let ((new (make-vector size 0)))
    (let loop ((i 0))
      (when (< i (vector-length v))
        (vector-set! new i (vector-ref v i))
        (loop (+ i 1))))
    new))

(define (compute-divisor-sum k)
  "s(k) from the cached a(d); needs a(1..k)."
  (let loop ((d 1) (sum 0))
    (cond ((> (* d d) k) sum)
          ((= 0 (remainder k d))
           (let ((e (quotient k d)))
             (loop (+ d 1)
                   (+ sum
                      (* d (vector-ref a-cache d))
                      (if (= e d) 0 (* e (vector-ref a-cache e)))))))
          (else (loop (+ d 1) sum)))))

(define (ensure! n)
  (when (> n known)
    (when (>= n (vector-length a-cache))
      (let ((size (max (+ n 1) (* 2 (vector-length a-cache)))))
        (set! a-cache (grow a-cache size))
        (set! s-cache (grow s-cache size))))
    (let loop ((m (+ known 1)))
      (when (<= m n)
        ;; a(m) = (1/(m-1)) * sum_{k=1..m-1} s(k) * a(m-k)
        (let inner ((k 1) (sum 0))
          (if (< k m)
              (inner (+ k 1)
                     (+ sum (* (vector-ref s-cache k)
                               (vector-ref a-cache (- m k)))))
              (vector-set! a-cache m (quotient sum (- m 1)))))
        (vector-set! s-cache m (compute-divisor-sum m))
        (set! known m)
        (loop (+ m 1))))))

(define (check-index who n)
  (unless (and (integer? n) (exact? n) (>= n 0))
    (error (string-append who ": index must be a non-negative exact integer:")
           n)))

(define (a000081 n)
  "The number of unlabelled rooted trees with N nodes."
  (check-index "a000081" n)
  (ensure! n)
  (vector-ref a-cache n))

(define (a000081-list count)
  "The list (a(0) a(1) ... a(COUNT-1))."
  (check-index "a000081-list" count)
  (let loop ((i (- count 1)) (acc '()))
    (if (< i 0) acc (loop (- i 1) (cons (a000081 i) acc)))))

(define (a000081-divisor-sum k)
  "s(K) = sum over divisors d of K of d*a(d), for K >= 1."
  (unless (and (integer? k) (exact? k) (>= k 1))
    (error "a000081-divisor-sum: index must be a positive exact integer:" k))
  (ensure! k)
  (vector-ref s-cache k))

(define (a000081-series n)
  "The generating function A(x) = sum a(i) x^i as a series of precision N."
  (make-series n a000081))

;;; Asymptotics: a(n) ~ C * alpha^n * n^(-3/2)  (Otter 1948).
;;;
;;; rho = 1/alpha is the radius of convergence of A and satisfies A(rho) = 1.
;;; The power series converges slowly at rho itself, so A(rho) = 1 is used in
;;; the functional equation A(x) = x exp(A(x) + sum_{k>=2} A(x^k)/k):
;;;
;;;   rho * exp(1 + sum_{k>=2} A(rho^k)/k) = 1,
;;;
;;; where every A(rho^k), k >= 2, is evaluated well inside the disc.  Writing
;;; A(x) = 1 - b sqrt(1 - x/rho) + ... near rho gives
;;;
;;;   b^2 = 2 (1 + sum_{k>=2} rho^k A'(rho^k)),   C = b / (2 sqrt(pi)).

(define eval-terms 200)

(define (eval-a x)
  "A(x) in floating point, for 0 <= x well below rho."
  (let loop ((i eval-terms) (sum 0.0))
    (if (< i 1)
        (* sum x)
        (loop (- i 1) (+ (* sum x) (exact->inexact (a000081 i)))))))

(define (eval-x-da x)
  "x * A'(x) in floating point, for 0 <= x well below rho."
  (let loop ((i eval-terms) (sum 0.0))
    (if (< i 1)
        (* sum x)
        (loop (- i 1) (+ (* sum x) (* i (exact->inexact (a000081 i))))))))

(define (tail-sum term x)
  "sum over k >= 2 of (TERM k x^k), stopping once x^k underflows the sum."
  (let loop ((k 2) (xk (* x x)) (sum 0.0))
    (if (< xk 1e-18)
        sum
        (loop (+ k 1) (* xk x) (+ sum (term k xk))))))

(define (rho-residual x)
  (- (* x (exp (+ 1.0 (tail-sum (lambda (k xk) (/ (eval-a xk) k))
                                x))))
     1.0))

(define rho-value #f)

(define (a000081-rho)
  "The radius of convergence rho of A(x), the root of A(rho) = 1."
  (unless rho-value
    ;; The residual is increasing on [0.3, 0.4] and changes sign there.
    (set! rho-value
          (let loop ((lo 0.3) (hi 0.4) (i 0))
            (let ((mid (/ (+ lo hi) 2.0)))
              (if (or (= i 200) (= mid lo) (= mid hi))
                  mid
                  (if (< (rho-residual mid) 0.0)
                      (loop mid hi (+ i 1))
                      (loop lo mid (+ i 1))))))))
  rho-value)

(define (a000081-alpha)
  "The growth rate alpha = lim a(n+1)/a(n) = 1/rho."
  (/ 1.0 (a000081-rho)))

(define (a000081-otter-constant)
  "The constant C in a(n) ~ C * alpha^n * n^(-3/2)."
  (let* ((rho (a000081-rho))
         (pi (* 4.0 (atan 1.0)))
         (b2 (* 2.0 (+ 1.0 (tail-sum (lambda (k xk) (eval-x-da xk)) rho)))))
    (/ (sqrt b2) (* 2.0 (sqrt pi)))))

(define (a000081-asymptotic n)
  "Otter's approximation C * alpha^N * N^(-3/2), for N >= 1."
  (unless (and (integer? n) (>= n 1))
    (error "a000081-asymptotic: n must be a positive integer:" n))
  (* (a000081-otter-constant)
     (expt (a000081-alpha) n)
     (expt (exact->inexact n) -1.5)))
