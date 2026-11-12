(use-modules (srfi srfi-64) (tests support) (ghost series))

(test-begin "series")

(define n 12)
(define (s . coeffs) (list->series n coeffs))

(test-equal "precision" n (series-precision (series-zero n)))
(test-equal "ref beyond precision" 0 (series-ref (series-one n) (+ n 5)))
(test-equal "add" (s 1 3 3) (series-add (s 1 2) (s 0 1 3)))
(test-equal "sub" (s 1 1 -3) (series-sub (s 1 2) (s 0 1 3)))
(test-equal "scale" (s 1/2 1) (series-scale 1/2 (s 1 2)))
(test-equal "mul" (s 1 2 1) (series-mul (s 1 1) (s 1 1)))
(test-equal "mul truncates"
  (series-zero 2)
  (series-mul (list->series 2 '(0 0 1)) (list->series 2 '(0 1))))
(test-equal "shift" (s 0 0 1 2) (series-shift (s 1 2) 2))
(test-equal "stretch" (s 1 0 0 2 0 0 3) (series-stretch (s 1 2 3) 3))

;; exp(x) has coefficients 1/i!.
(test-equal "exp of x"
  (let loop ((i 0) (f 1) (acc '()))
    (if (> i n) (reverse acc) (loop (+ i 1) (* f (+ i 1)) (cons (/ 1 f) acc))))
  (series->list (series-exp (series-x n))))

;; log(1/(1-x)) = sum x^i / i.
(test-equal "log of geometric series"
  (make-series n (lambda (i) (if (= i 0) 0 (/ 1 i))))
  (series-log (make-series n (lambda (i) 1))))

(let ((g (s 0 3 -1/2 5 7/3 0 2)))
  (test-equal "log inverts exp" g (series-log (series-exp g))))
(let ((f (s 1 3 -1/2 5 7/3 0 2)))
  (test-equal "exp inverts log" f (series-exp (series-log f))))
(let ((g (s 0 1 2 3)) (h (s 0 -4 1/3)))
  (test-equal "exp is a homomorphism"
    (series-mul (series-exp g) (series-exp h))
    (series-exp (series-add g h))))

(test-equal "geometric power m=0" (series-one n) (series-geometric-power n 2 0))
(test-equal "geometric power m=1"
  (make-series n (lambda (i) (if (even? i) 1 0)))
  (series-geometric-power n 2 1))
(test-equal "geometric power is a repeated product"
  (let ((g (series-geometric-power n 3 1)))
    (series-mul g (series-mul g (series-mul g g))))
  (series-geometric-power n 3 4))
(test-equal "geometric power inverts (1-x^k)^m"
  (series-one n)
  (let ((p (s 1 0 -1)))
    (series-mul (series-mul p (series-mul p p))
                (series-geometric-power n 2 3))))

(test-error "exp needs zero constant term" #t (series-exp (s 1 1)))
(test-error "log needs unit constant term" #t (series-log (s 2 1)))
(test-error "inexact coefficients rejected" #t (list->series 3 '(0.5)))

(test-finish "series")
