;;; (ghost butcher) - Runge-Kutta order conditions over rooted trees
;;;
;;; A tableau is a pair (A . b): A is a list of s rows of s exact rationals
;;; and b is a list of s exact rationals.  The abscissae c_i are the row
;;; sums of A.  A method has order p exactly when its elementary weight
;;; satisfies Phi(t) = 1/gamma(t) for every rooted tree t with |t| <= p, one
;;; condition per tree, so there are a(1) + ... + a(p) conditions.

(define-module (ghost butcher)
  #:use-module (srfi srfi-1)
  #:use-module (ghost tree)
  #:export (make-tableau
            tableau-a
            tableau-b
            tableau-c
            tableau-stages
            elementary-weight
            order-condition-holds?
            order-conditions
            failed-order-conditions
            rk-order
            euler-tableau
            midpoint-tableau
            heun-tableau
            kutta3-tableau
            rk4-tableau
            three-eighths-tableau
            implicit-midpoint-tableau))

(define (make-tableau a b)
  "Build a tableau from the list of rows A and the weights B."
  (let ((s (length b)))
    (unless (and (= s (length a))
                 (every (lambda (row) (= s (length row))) a))
      (error "make-tableau: A must be square with one row per weight"))
    (unless (every (lambda (x) (and (rational? x) (exact? x)))
                   (append b (concatenate a)))
      (error "make-tableau: coefficients must be exact rationals"))
    (cons a b)))

(define (tableau-a tab) (car tab))
(define (tableau-b tab) (cdr tab))
(define (tableau-stages tab) (length (cdr tab)))

(define (tableau-c tab)
  "The abscissae c_i = sum_j a_ij."
  (map (lambda (row) (apply + row)) (tableau-a tab)))

(define (stage-weights tab t)
  ;; The list over stages i of phi_i(t), with phi_i(single node) = 1 and
  ;; phi_i([t1 .. tk]) = product over j of sum_l a_il phi_l(tj).
  (fold (lambda (child acc)
          (let ((inner (stage-weights tab child)))
            (map (lambda (row prod)
                   (* prod (apply + (map * row inner))))
                 (tableau-a tab)
                 acc)))
        (make-list (tableau-stages tab) 1)
        t))

(define (elementary-weight tab t)
  "Phi(t) = sum_i b_i phi_i(t) for the tableau TAB and the tree T."
  (apply + (map * (tableau-b tab) (stage-weights tab t))))

(define (order-condition-holds? tab t)
  (= (elementary-weight tab t) (/ 1 (tree-density t))))

(define (order-conditions p)
  "The trees with at most P nodes: one order condition each."
  (append-map trees-of-size (iota p 1)))

(define (failed-order-conditions tab p)
  "The trees with at most P nodes whose order condition TAB violates."
  (remove (lambda (t) (order-condition-holds? tab t))
          (order-conditions p)))

(define* (rk-order tab #:optional (max-order 8))
  "The order of TAB: the largest p <= MAX-ORDER such that every order
condition for trees with at most p nodes holds."
  (let loop ((p 0))
    (if (and (< p max-order)
             (every (lambda (t) (order-condition-holds? tab t))
                    (trees-of-size (+ p 1))))
        (loop (+ p 1))
        p)))

(define (euler-tableau)
  (make-tableau '((0)) '(1)))

(define (midpoint-tableau)
  (make-tableau '((0 0) (1/2 0)) '(0 1)))

(define (heun-tableau)
  (make-tableau '((0 0) (1 0)) '(1/2 1/2)))

(define (kutta3-tableau)
  (make-tableau '((0 0 0) (1/2 0 0) (-1 2 0)) '(1/6 2/3 1/6)))

(define (rk4-tableau)
  (make-tableau '((0 0 0 0) (1/2 0 0 0) (0 1/2 0 0) (0 0 1 0))
                '(1/6 1/3 1/3 1/6)))

(define (three-eighths-tableau)
  (make-tableau '((0 0 0 0) (1/3 0 0 0) (-1/3 1 0 0) (1 -1 1 0))
                '(1/8 3/8 3/8 1/8)))

(define (implicit-midpoint-tableau)
  (make-tableau '((1/2)) '(1)))
