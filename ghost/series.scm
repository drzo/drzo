;;; (ghost series) - truncated formal power series over the exact rationals
;;;
;;; A series of precision N is a vector of N+1 exact rationals holding the
;;; coefficients of x^0 .. x^N.  Every operation truncates at x^N.

(define-module (ghost series)
  #:export (make-series
            series-precision
            series-ref
            list->series
            series->list
            series-zero
            series-one
            series-x
            series=?
            series-add
            series-sub
            series-scale
            series-mul
            series-shift
            series-stretch
            series-exp
            series-log
            series-geometric-power))

(define (make-series n proc)
  "Return the series of precision N whose x^i coefficient is (PROC i)."
  (let ((v (make-vector (+ n 1) 0)))
    (let loop ((i 0))
      (when (<= i n)
        (let ((c (proc i)))
          (unless (and (rational? c) (exact? c))
            (error "series coefficient is not an exact rational:" c))
          (vector-set! v i c))
        (loop (+ i 1))))
    v))

(define (series-precision s)
  (- (vector-length s) 1))

(define (series-ref s i)
  "Coefficient of x^I in S; 0 beyond the truncation order."
  (if (and (>= i 0) (< i (vector-length s)))
      (vector-ref s i)
      0))

(define (list->series n coeffs)
  "Series of precision N with leading coefficients COEFFS (x^0 first)."
  (let ((v (list->vector coeffs)))
    (make-series n (lambda (i) (if (< i (vector-length v)) (vector-ref v i) 0)))))

(define (series->list s)
  (vector->list s))

(define (series-zero n) (make-series n (lambda (i) 0)))
(define (series-one n) (make-series n (lambda (i) (if (= i 0) 1 0))))
(define (series-x n) (make-series n (lambda (i) (if (= i 1) 1 0))))

(define (series=? a b)
  (equal? a b))

(define (common-precision a b)
  (min (series-precision a) (series-precision b)))

(define (series-add a b)
  (make-series (common-precision a b)
               (lambda (i) (+ (vector-ref a i) (vector-ref b i)))))

(define (series-sub a b)
  (make-series (common-precision a b)
               (lambda (i) (- (vector-ref a i) (vector-ref b i)))))

(define (series-scale c a)
  (make-series (series-precision a) (lambda (i) (* c (vector-ref a i)))))

(define (series-mul a b)
  (make-series
   (common-precision a b)
   (lambda (i)
     (let loop ((j 0) (sum 0))
       (if (> j i)
           sum
           (loop (+ j 1)
                 (+ sum (* (vector-ref a j) (vector-ref b (- i j))))))))))

(define (series-shift a k)
  "x^K * A, truncated to the precision of A.  K must be non-negative."
  (when (< k 0) (error "series-shift: negative shift:" k))
  (make-series (series-precision a)
               (lambda (i) (if (>= i k) (vector-ref a (- i k)) 0))))

(define (series-stretch a k)
  "A(x^K), truncated to the precision of A.  K must be positive."
  (unless (and (integer? k) (> k 0))
    (error "series-stretch: exponent must be a positive integer:" k))
  (make-series (series-precision a)
               (lambda (i)
                 (if (= 0 (remainder i k))
                     (vector-ref a (quotient i k))
                     0))))

(define (series-exp g)
  "exp(G) for a series G with zero constant term."
  (unless (= 0 (vector-ref g 0))
    (error "series-exp: constant term must be 0, got:" (vector-ref g 0)))
  ;; f = exp(g) satisfies f' = g' f, so n f_n = sum_{k=1..n} k g_k f_{n-k}.
  (let* ((n (series-precision g))
         (f (make-vector (+ n 1) 0)))
    (vector-set! f 0 1)
    (let loop ((i 1))
      (when (<= i n)
        (let inner ((k 1) (sum 0))
          (if (> k i)
              (vector-set! f i (/ sum i))
              (inner (+ k 1)
                     (+ sum (* k (vector-ref g k) (vector-ref f (- i k)))))))
        (loop (+ i 1))))
    f))

(define (series-log f)
  "log(F) for a series F with constant term 1."
  (unless (= 1 (vector-ref f 0))
    (error "series-log: constant term must be 1, got:" (vector-ref f 0)))
  ;; g = log(f) satisfies f' = g' f, so
  ;; n g_n = n f_n - sum_{k=1..n-1} k g_k f_{n-k}.
  (let* ((n (series-precision f))
         (g (make-vector (+ n 1) 0)))
    (let loop ((i 1))
      (when (<= i n)
        (let inner ((k 1) (sum (* i (vector-ref f i))))
          (if (>= k i)
              (vector-set! g i (/ sum i))
              (inner (+ k 1)
                     (- sum (* k (vector-ref g k) (vector-ref f (- i k)))))))
        (loop (+ i 1))))
    g))

(define (series-geometric-power n k m)
  "(1 - x^K)^(-M) to precision N for a positive integer K and a
non-negative integer M, from the binomial series
sum_j C(M+j-1, j) x^(K j)."
  (unless (and (integer? k) (> k 0))
    (error "series-geometric-power: k must be a positive integer:" k))
  (unless (and (integer? m) (exact? m) (>= m 0))
    (error "series-geometric-power: m must be a non-negative integer:" m))
  (let ((v (make-vector (+ n 1) 0)))
    ;; C(m+j-1, j) = C(m+j-2, j-1) * (m+j-1) / j
    (let loop ((j 0) (c 1))
      (when (<= (* j k) n)
        (vector-set! v (* j k) c)
        (loop (+ j 1) (/ (* c (+ m j)) (+ j 1)))))
    v))
