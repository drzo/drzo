(use-modules (srfi srfi-1) (srfi srfi-64)
             (tests support) (ghost a000081) (ghost tree) (ghost butcher))

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

(test-finish "butcher")
