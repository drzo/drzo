(use-modules (srfi srfi-1) (srfi srfi-64)
             (tests support) (ghost series) (ghost a000081) (ghost tree)
             (ghost butcher))

(test-begin "butcher")

(test-equal "number of order conditions through order p"
  '(1 2 4 8 17 37)
  (map (lambda (p) (length (order-conditions p))) (iota 6 1)))
(test-equal "which is the partial sum of a(k)"
  (map (lambda (p) (apply + (map a000081 (iota p 1)))) (iota 8 1))
  (map (lambda (p) (length (order-conditions p))) (iota 8 1)))

(test-equal "abscissae of RK4" '(0 1/2 1/2 1) (tableau-c (rk4-tableau)))
(test-equal "stages of RK4" 4 (tableau-stages (rk4-tableau)))

;; Elementary weights by hand for RK4.
(test-equal "sum b_i" 1 (elementary-weight (rk4-tableau) '()))
(test-equal "sum b_i c_i" 1/2 (elementary-weight (rk4-tableau) '(())))
(test-equal "sum b_i c_i^2" 1/3 (elementary-weight (rk4-tableau) '(() ())))
(test-equal "sum b_i a_ij c_j" 1/6 (elementary-weight (rk4-tableau) '((()))))
(test-equal "sum b_i c_i^4, where RK4 falls short of 1/5"
  5/24
  (elementary-weight (rk4-tableau) '(() () () ())))
(test-equal "weights do not depend on child order"
  (elementary-weight (three-eighths-tableau) '((()) () (() ())))
  (elementary-weight (three-eighths-tableau) '((() ()) (()) ())))

;; Known orders.
(test-equal "Euler" 1 (rk-order (euler-tableau)))
(test-equal "explicit midpoint" 2 (rk-order (midpoint-tableau)))
(test-equal "Heun" 2 (rk-order (heun-tableau)))
(test-equal "Kutta third order" 3 (rk-order (kutta3-tableau)))
(test-equal "classical RK4" 4 (rk-order (rk4-tableau)))
(test-equal "3/8 rule" 4 (rk-order (three-eighths-tableau)))
(test-equal "implicit midpoint" 2 (rk-order (implicit-midpoint-tableau)))
(test-equal "max-order caps the search" 2 (rk-order (rk4-tableau) 2))
(test-equal "inconsistent method has order 0"
  0
  (rk-order (make-tableau '((0)) '(1/2))))

(test-equal "RK4 satisfies all 8 conditions through order 4"
  '()
  (failed-order-conditions (rk4-tableau) 4))
(test-assert "RK4 fails at least one of the 9 conditions of order 5"
  (let ((failed (failed-order-conditions (rk4-tableau) 5)))
    (and (pair? failed)
         (every (lambda (t) (= 5 (tree-size t))) failed))))
(test-equal "Heun fails exactly the two order-3 conditions"
  (trees-of-size 3)
  (failed-order-conditions (heun-tableau) 3))

(test-error "non-square tableau" #t (make-tableau '((0 0)) '(1)))
(test-error "inexact tableau" #t (make-tableau '((0.0)) '(1)))

(test-equal "Dormand-Prince" 5 (rk-order (dormand-prince-tableau)))
(test-equal "Dormand-Prince satisfies all 17 conditions through order 5"
  '()
  (failed-order-conditions (dormand-prince-tableau) 5))
(test-assert "RK4 is explicit" (tableau-explicit? (rk4-tableau)))
(test-assert "Dormand-Prince is explicit"
  (tableau-explicit? (dormand-prince-tableau)))
(test-assert "implicit midpoint is not explicit"
  (not (tableau-explicit? (implicit-midpoint-tableau))))

;;; Elementary differentials.

(define (factorial n) (if (< n 2) 1 (* n (factorial (- n 1)))))

;; f(y) = y^2 at y = 3: f = 9, f' = 6, f'' = 2, and the third derivative is 0.
(define square (scalar-field '(0 0 1)))

(test-equal "F(.) = f" '(9) (elementary-differential square '() '(3)))
(test-equal "F([.]) = f' f" '(54) (elementary-differential square '(()) '(3)))
(test-equal "F([.,.]) = f'' f f" '(162)
  (elementary-differential square '(() ()) '(3)))
(test-equal "F([[.]]) = f' f' f" '(324)
  (elementary-differential square '((())) '(3)))
(test-equal "F([.,.,.]) = 0: the third derivative of y^2 vanishes" '(0)
  (elementary-differential square '(() () ()) '(3)))

;; For a scalar field F(t) is the product over the nodes of t of the
;; derivative of f whose order is the node's number of children.
(define cubic-coefficients '(1/2 -1 3 2/3))
(define cubic (scalar-field cubic-coefficients))

(define (polynomial-derivative coefficients)
  (if (null? coefficients)
      '()
      (map * (cdr coefficients) (iota (- (length coefficients) 1) 1))))

(define (nth-derivative-value coefficients k y)
  (let ((d (fold (lambda (i c) (polynomial-derivative c)) coefficients (iota k))))
    (fold (lambda (c i sum) (+ sum (* c (expt y i)))) 0 d (iota (length d)))))

(define (node-product t y)
  (apply * (nth-derivative-value cubic-coefficients (length t) y)
         (map (lambda (child) (node-product child y)) t)))

(test-assert "scalar F(t) is a product over nodes, trees up to 8 nodes"
  (every (lambda (t)
           (equal? (list (node-product t 2/5))
                   (elementary-differential cubic t '(2/5))))
         (order-conditions 8)))

;; A two-dimensional field by hand: f = (x y, x + y^2) at (2, 3).
;; f = (6, 11); J = ((3 2) (1 6)); f''(u, v) = (u1 v2 + u2 v1, 2 u2 v2).
(define hand-field (polynomial-field '(((1 1 1)) ((1 1 0) (1 0 2)))))

(test-equal "2-D: F(.)" '(6 11) (elementary-differential hand-field '() '(2 3)))
(test-equal "2-D: F([.]) = J f" '(40 72)
  (elementary-differential hand-field '(()) '(2 3)))
(test-equal "2-D: F([.,.]) = f''(f, f)" '(132 242)
  (elementary-differential hand-field '(() ()) '(2 3)))
(test-equal "2-D: F([[.]]) = J J f" '(264 472)
  (elementary-differential hand-field '((())) '(2 3)))
(test-equal "2-D: F([[.],.]) = f''(J f, f)" '(872 1584)
  (elementary-differential hand-field '((()) ()) '(2 3)))
(test-equal "F does not depend on child order"
  (elementary-differential hand-field '((()) () (() ())) '(2 3))
  (elementary-differential hand-field '((() ()) (()) ()) '(2 3)))
(test-equal "a field of degree 2 kills trees with a node of 3 children"
  '(0 0)
  (elementary-differential hand-field '((() () ()) ()) '(2 3)))

;; Linear fields: F(chain of n nodes) = A^n y, and every other tree gives 0.
(define matrix '((1 2) (-3 1/2)))
(define linear (linear-field matrix))

(define (matrix-apply m v)
  (map (lambda (row) (apply + (map * row v))) m))

(define (matrix-power-apply m n v)
  (if (= n 0) v (matrix-power-apply m (- n 1) (matrix-apply m v))))

(test-assert "linear: F(chain) = A^n y"
  (every (lambda (n)
           (equal? (matrix-power-apply matrix n '(1 -2))
                   (elementary-differential linear (chain-tree n) '(1 -2))))
         (iota 8 1)))
(test-assert "linear: F vanishes on every tree that is not a chain"
  (every (lambda (t)
           (or (equal? t (chain-tree (tree-size t)))
               (equal? '(0 0) (elementary-differential linear t '(1 -2)))))
         (order-conditions 7)))

(test-error "polynomial term with the wrong number of exponents" #t
  (polynomial-field '(((1 1)) ((1 0 1)))))
(test-error "non-square linear field" #t (linear-field '((1 2))))
(test-error "point of the wrong dimension" #t
  (elementary-differential hand-field '() '(1)))

;;; The exact flow as a B-series, against Taylor coefficients computed
;;; without trees: substitute a power series into y' = f(y) and solve
;;; coefficient by coefficient with (ghost series).

(define (polynomial-of-series p ys n)
  (fold (lambda (term sum)
          (series-add
           sum
           (series-scale
            (car term)
            (fold (lambda (y e product)
                    (fold (lambda (i product) (series-mul product y))
                          product
                          (iota e)))
                  (series-one n)
                  ys
                  (cdr term)))))
        (series-zero n)
        p))

(define (series-solution components y0 n)
  "The Taylor coefficients c_0 .. c_n of the solution through Y0, as a
list of vectors (lists), from (k+1) c_(k+1) = [t^k] f(y(t))."
  (let loop ((k 0) (coefficients (list y0)))
    (if (= k n)
        (reverse coefficients)
        (let* ((known (reverse coefficients))
               (ys (map (lambda (i)
                          (list->series n (map (lambda (c) (list-ref c i)) known)))
                        (iota (length y0))))
               (next (map (lambda (p)
                            (/ (series-ref (polynomial-of-series p ys n) k)
                               (+ k 1)))
                          components)))
          (loop (+ k 1) (cons next coefficients))))))

(define (test-against-series name components y0 order)
  (let ((field (polynomial-field components))
        (coefficients (series-solution components y0 order)))
    (test-equal (string-append name ": y^(k) = k! c_k for k up to "
                               (number->string order))
      (map (lambda (c k) (map (lambda (x) (* x (factorial k))) c))
           coefficients
           (iota (+ order 1)))
      (solution-jet field y0 order))))

(define lotka-volterra '(((1 1 0) (-1 1 1)) ((-1 0 1) (1 1 1))))
(define van-der-pol '(((1 0 1)) ((1 0 1) (-1 2 1) (-1 1 0))))
(define rigid-body '(((-1/2 0 1 1)) ((2/3 1 0 1)) ((-1/5 1 1 0))))

(test-against-series "Lotka-Volterra" lotka-volterra '(2 1/3) 9)
(test-against-series "van der Pol" van-der-pol '(2 -1/2) 9)
(test-against-series "rigid body" rigid-body '(1 1/2 -1/3) 9)
(test-against-series "scalar cubic" (list (map list cubic-coefficients (iota 4)))
                     '(2/5) 9)

;; y' = y^2 from 1 is 1/(1 - t): every Taylor coefficient is 1.
(test-equal "y' = y^2: y^(k)(0) = k!"
  (map (lambda (k) (list (factorial k))) (iota 10))
  (solution-jet square '(1) 9))

;; y' = 1 + y^2 from 0 is tan t; its derivatives at 0 are the tangent numbers.
(test-equal "y' = 1 + y^2: tangent numbers"
  '((0) (1) (0) (2) (0) (16) (0) (272) (0) (7936) (0))
  (solution-jet (scalar-field '(1 0 1)) '(0) 10))

;; y' = A y: the Taylor polynomial of exp(h A) y.
(test-equal "linear: Taylor flow is the truncated matrix exponential"
  (fold (lambda (n sum)
          (map + sum
               (map (lambda (x) (* x (/ (expt 1/3 n) (factorial n))))
                    (matrix-power-apply matrix n '(1 -2)))))
        '(0 0)
        (iota 9))
  (taylor-flow linear '(1 -2) 1/3 8))

(test-equal "Taylor flow is sum h^k/k! y^(k)"
  (fold (lambda (dy k sum)
          (map + sum (map (lambda (x) (* x (/ (expt 2/7 k) (factorial k)))) dy)))
        '(0 0)
        (solution-jet hand-field '(2 3) 7)
        (iota 8))
  (taylor-flow hand-field '(2 3) 2/7 7))

(test-error "negative derivative order" #t (solution-derivative square '(1) -1))

;;; A Runge-Kutta step as a B-series.  For an explicit method with s stages
;;; Phi(t) = 0 once t has height s or more, and for a field of degree m
;;; F(t) = 0 once a node has more than m children, so the series is a finite
;;; sum and must equal the step exactly.

(define (multisets items k)
  (cond ((= k 0) '(()))
        ((null? items) '())
        (else (append (map (lambda (rest) (cons (car items) rest))
                           (multisets items (- k 1)))
                      (multisets (cdr items) k)))))

(define (bounded-trees height degree)
  "Every tree of height at most HEIGHT whose nodes have at most DEGREE children."
  (if (= height 0)
      '(())
      (let ((smaller (bounded-trees (- height 1) degree)))
        (map tree-canonical
             (append-map (lambda (k) (multisets smaller k))
                         (iota (+ degree 1)))))))

(test-equal "bounded trees: counts for degree 2" '(1 3 10 66)
  (map (lambda (h) (length (bounded-trees h 2))) (iota 4)))
(test-equal "bounded trees: no repeats" 66
  (length (delete-duplicates (bounded-trees 3 2))))
(test-equal "bounded trees: the largest has 15 nodes" 15
  (apply max (map tree-size (bounded-trees 3 2))))

(test-assert "explicit s-stage methods: Phi vanishes on trees of height >= s"
  (every (lambda (t)
           (or (< (tree-height t) 4)
               (= 0 (elementary-weight (rk4-tableau) t))))
         (order-conditions 9)))

(test-equal "RK4 step = finite B-series, scalar quadratic field"
  (rk-step (rk4-tableau) (scalar-field '(1/2 1 -3/4)) '(2/3) 1/5)
  (b-series-sum (scalar-field '(1/2 1 -3/4)) (rk-coefficient (rk4-tableau))
                '(2/3) 1/5 (bounded-trees 3 2)))
(test-equal "RK4 step = finite B-series, Lotka-Volterra"
  (rk-step (rk4-tableau) (polynomial-field lotka-volterra) '(2 1/3) 1/4)
  (b-series-sum (polynomial-field lotka-volterra) (rk-coefficient (rk4-tableau))
                '(2 1/3) 1/4 (bounded-trees 3 2)))
(test-equal "3/8-rule step = finite B-series, rigid body"
  (rk-step (three-eighths-tableau) (polynomial-field rigid-body) '(1 1/2 -1/3) 1/2)
  (b-series-sum (polynomial-field rigid-body)
                (rk-coefficient (three-eighths-tableau))
                '(1 1/2 -1/3) 1/2 (bounded-trees 3 2)))
(test-equal "Kutta step = finite B-series, van der Pol (degree 3)"
  (rk-step (kutta3-tableau) (polynomial-field van-der-pol) '(2 -1/2) 1/3)
  (b-series-sum (polynomial-field van-der-pol) (rk-coefficient (kutta3-tableau))
                '(2 -1/2) 1/3 (bounded-trees 2 3)))
(test-equal "RK4 step on a linear field: chains of at most 4 nodes"
  (rk-step (rk4-tableau) linear '(1 -2) 1/3)
  (b-series linear (rk-coefficient (rk4-tableau)) '(1 -2) 1/3 4))
(test-equal "which is the degree-4 Taylor polynomial of the flow"
  (taylor-flow linear '(1 -2) 1/3 4)
  (rk-step (rk4-tableau) linear '(1 -2) 1/3))
(test-equal "Euler step" '(7/2 23/4)
  (rk-step (euler-tableau) hand-field '(2 3) 1/4))
(test-error "implicit methods cannot be stepped" #t
  (rk-step (implicit-midpoint-tableau) square '(1) 1/10))

;; The terms of order q of a method and of the exact flow agree for q up to
;; the order of the method and differ at the next.
(define (order-term field coefficient y q)
  (map - (b-series-sum field coefficient y 1 (trees-of-size q)) y))

(let ((field (polynomial-field lotka-volterra)) (y '(2 1/3)))
  (do ((q 1 (+ q 1))) ((> q 4))
    (test-equal (format #f "RK4 and the flow agree at order ~a" q)
      (order-term field exact-flow-coefficient y q)
      (order-term field (rk-coefficient (rk4-tableau)) y q)))
  (test-assert "RK4 and the flow differ at order 5"
    (not (equal? (order-term field exact-flow-coefficient y 5)
                 (order-term field (rk-coefficient (rk4-tableau)) y 5))))
  (do ((q 1 (+ q 1))) ((> q 5))
    (test-equal (format #f "Dormand-Prince and the flow agree at order ~a" q)
      (order-term field exact-flow-coefficient y q)
      (order-term field (rk-coefficient (dormand-prince-tableau)) y q)))
  (test-assert "Dormand-Prince and the flow differ at order 6"
    (not (equal? (order-term field exact-flow-coefficient y 6)
                 (order-term field (rk-coefficient (dormand-prince-tableau)) y 6)))))

;;; J-surfaces in floating point: the Taylor method of degree 8 against
;;; closed forms and against a fifth-order reference integration with a much
;;; smaller step.

(define (close? x y tolerance) (< (abs (- x y)) tolerance))
(define (all-close? xs ys tolerance)
  (every (lambda (x y) (close? x y tolerance)) xs ys))

(test-assert "y' = y^2 from 1/2 reaches 1 at t = 1"
  (all-close? '(1.0) (taylor-integrate square '(0.5) 1/32 8 32) 1e-10))
(test-assert "y' = 1 + y^2 from 0 reaches tan 1 at t = 1"
  (all-close? (list (tan 1.0))
              (taylor-integrate (scalar-field '(1 0 1)) '(0.0) 1/64 8 64)
              1e-10))
(test-assert "harmonic oscillator reaches (cos 2, -sin 2) at t = 2"
  (all-close? (list (cos 2.0) (- (sin 2.0)))
              (taylor-integrate (linear-field '((0 1) (-1 0))) '(1.0 0.0) 1/16 8 32)
              1e-12))

(define (test-against-reference name components y0)
  (let* ((field (polynomial-field components))
         (taylor (taylor-integrate field y0 1/32 8 64))
         (reference (rk-integrate (dormand-prince-tableau) field y0 1/512 1024))
         (coarse (rk-integrate (dormand-prince-tableau) field y0 1/8 16)))
    (test-assert (string-append name ": Taylor method agrees with the reference at t = 2")
      (all-close? reference taylor 1e-10))
    ;; The comparison has teeth: the reference method at a coarse step is
    ;; visibly further away.
    (test-assert (string-append name ": the reference method at a coarse step does not")
      (not (all-close? reference coarse 1e-9)))))

(test-against-reference "Lotka-Volterra" lotka-volterra '(2.0 1.0))
(test-against-reference "van der Pol" van-der-pol '(2.0 0.0))

;; The Taylor method of degree p converges with order p: halving the step
;; divides the error at t = 2 by about 2^p.
(let* ((field (polynomial-field lotka-volterra))
       (start '(2.0 1.0))
       (reference (rk-integrate (dormand-prince-tableau) field start 1/512 1024))
       (error-at (lambda (p h steps)
                   (apply max
                          (map (lambda (x y) (abs (- x y)))
                               reference
                               (taylor-integrate field start h p steps))))))
  (do ((p 2 (+ p 1))) ((> p 6))
    (let ((observed (/ (log (/ (error-at p 1/8 16) (error-at p 1/16 32)))
                       (log 2.0))))
      (test-assert (format #f "Taylor method of degree ~a converges with order ~a" p p)
        (close? observed p 0.25)))))

;; Lotka-Volterra conserves x - log x + y - log y.
(let* ((invariant (lambda (p) (+ (- (car p) (log (car p)))
                                 (- (cadr p) (log (cadr p))))))
       (start '(2.0 1.0))
       (end (taylor-integrate (polynomial-field lotka-volterra) start 1/32 8 64)))
  (test-assert "Lotka-Volterra: the Taylor method keeps the invariant"
    (close? (invariant start) (invariant end) 1e-10))
  (test-assert "Lotka-Volterra: the solution has moved"
    (not (all-close? start end 1e-1))))

(test-finish "butcher")
