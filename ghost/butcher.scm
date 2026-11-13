;;; (ghost butcher) - Runge-Kutta order conditions and B-series over rooted trees
;;;
;;; A tableau is a pair (A . b): A is a list of s rows of s exact rationals
;;; and b is a list of s exact rationals.  The abscissae c_i are the row
;;; sums of A.  A method has order p exactly when its elementary weight
;;; satisfies Phi(t) = 1/gamma(t) for every rooted tree t with |t| <= p, one
;;; condition per tree, so there are a(1) + ... + a(p) conditions.
;;;
;;; For y' = f(y) the elementary differential of a tree t = [t1 .. tk] is
;;;
;;;   F(t)(y) = f^(k)(y)(F(t1)(y), ..., F(tk)(y)),   F(single node)(y) = f(y),
;;;
;;; and the B-series with coefficient map a is
;;;
;;;   B(a, y) = y + sum over trees t of h^|t| / sigma(t) * a(t) * F(t)(y).
;;;
;;; The exact flow over a time h is the B-series with a(t) = 1/gamma(t), and
;;; one step of a Runge-Kutta method is the B-series with a(t) = Phi(t).

(define-module (ghost butcher)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ghost tree)
  #:export (make-tableau
            tableau-a
            tableau-b
            tableau-c
            tableau-stages
            tableau-explicit?
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
            implicit-midpoint-tableau
            dormand-prince-tableau

            make-field
            field?
            field-dimension
            field-apply
            polynomial-field
            linear-field
            scalar-field
            elementary-differential

            b-series-sum
            b-series
            exact-flow-coefficient
            rk-coefficient
            rk-step
            rk-integrate

            solution-derivative
            solution-jet
            taylor-flow
            taylor-integrate))

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

(define (dormand-prince-tableau)
  "The fifth-order method of the Dormand-Prince 5(4) pair."
  (make-tableau
   '((0 0 0 0 0 0 0)
     (1/5 0 0 0 0 0 0)
     (3/40 9/40 0 0 0 0 0)
     (44/45 -56/15 32/9 0 0 0 0)
     (19372/6561 -25360/2187 64448/6561 -212/729 0 0 0)
     (9017/3168 -355/33 46732/5247 49/176 -5103/18656 0 0)
     (35/384 0 500/1113 125/192 -2187/6784 11/84 0))
   '(35/384 0 500/1113 125/192 -2187/6784 11/84 0)))

(define (tableau-explicit? tab)
  "True if A is strictly lower triangular, so each stage needs only the
stages before it."
  (every (lambda (row i) (every zero? (drop row i)))
         (tableau-a tab)
         (iota (tableau-stages tab))))

;;; Vector fields.
;;;
;;; A field carries its dimension d and a procedure giving all of its
;;; derivatives: (derivative y vectors) is the k-linear map f^(k)(y) applied
;;; to the k vectors in the list VECTORS, as a list of d numbers.  With no
;;; vectors it is f(y) itself.  Points and vectors are lists of d numbers,
;;; exact or inexact.

(define-record-type <field>
  (%make-field dimension derivative)
  field?
  (dimension field-dimension)
  (derivative field-derivative))

(define (make-field dimension derivative)
  "A vector field on DIMENSION variables with derivatives DERIVATIVE."
  (unless (and (integer? dimension) (exact? dimension) (>= dimension 1))
    (error "make-field: dimension must be a positive exact integer:" dimension))
  (unless (procedure? derivative)
    (error "make-field: derivative must be a procedure:" derivative))
  (%make-field dimension derivative))

(define (field-apply field y vectors)
  "f^(k)(Y) applied to the k vectors in VECTORS; f(Y) when VECTORS is empty."
  (unless (= (length y) (field-dimension field))
    (error "field-apply: point has the wrong dimension:" y))
  ((field-derivative field) y vectors))

;;; Polynomial fields.  A polynomial in d variables is a list of terms
;;; (c e1 ... ed) standing for c * x1^e1 * ... * xd^ed.

(define (collect-terms p)
  ;; Merge terms with equal exponents and drop those that cancel exactly.
  (let loop ((p p) (acc '()))
    (if (null? p)
        (remove (lambda (term) (and (exact? (car term)) (zero? (car term))))
                (reverse acc))
        (let* ((term (car p))
               (same (find (lambda (seen) (equal? (cdr seen) (cdr term))) acc)))
          (if same
              (begin (set-car! same (+ (car same) (car term)))
                     (loop (cdr p) acc))
              (loop (cdr p) (cons (cons (car term) (cdr term)) acc)))))))

(define (directional-derivative p v)
  ;; sum_i v_i * dp/dx_i, for a constant vector v.
  (collect-terms
   (append-map
    (lambda (term)
      (let ((c (car term)) (exponents (cdr term)))
        (filter-map
         (lambda (i e vi)
           (and (> e 0)
                (not (and (exact? vi) (zero? vi)))
                (cons (* c e vi)
                      (append (take exponents i)
                              (list (- e 1))
                              (drop exponents (+ i 1))))))
         (iota (length exponents)) exponents v)))
    p)))

(define (polynomial-value p y)
  (fold (lambda (term sum)
          (+ sum (apply * (car term) (map expt y (cdr term)))))
        0
        p))

(define (polynomial-field components)
  "The field whose i-th component is the i-th polynomial of COMPONENTS.
Each polynomial is a list of terms (c e1 ... ed), d being the number of
components."
  (let ((d (length components)))
    (for-each
     (lambda (p)
       (for-each
        (lambda (term)
          (unless (and (pair? term)
                       (number? (car term))
                       (= d (length (cdr term)))
                       (every (lambda (e) (and (integer? e) (exact? e) (>= e 0)))
                              (cdr term)))
            (error "polynomial-field: bad term for this dimension:" term d)))
        p))
     components)
    (let ((polynomials (map collect-terms components)))
      (make-field
       d
       (lambda (y vectors)
         (map (lambda (p)
                (polynomial-value (fold (lambda (v p) (directional-derivative p v))
                                        p
                                        vectors)
                                  y))
              polynomials))))))

(define (linear-field matrix)
  "The field f(y) = MATRIX y, MATRIX being a list of d rows of d numbers."
  (let ((d (length matrix)))
    (unless (every (lambda (row) (= d (length row))) matrix)
      (error "linear-field: matrix must be square:" matrix))
    (polynomial-field
     (map (lambda (row)
            (map (lambda (entry j)
                   (cons entry (map (lambda (i) (if (= i j) 1 0)) (iota d))))
                 row (iota d)))
          matrix))))

(define (scalar-field coefficients)
  "The one-dimensional field f(y) = c0 + c1 y + c2 y^2 + ..., COEFFICIENTS
being the list (c0 c1 c2 ...)."
  (polynomial-field
   (list (map (lambda (c i) (list c i))
              coefficients
              (iota (length coefficients))))))

;;; Elementary differentials.

(define (elementary-differential field t y)
  "F(T)(Y) for FIELD: f(Y) for the single node, and otherwise the k-th
derivative of f at Y applied to the elementary differentials of the k root
subtrees of T."
  (field-apply field y
               (map (lambda (child) (elementary-differential field child y))
                    t)))

(define (differential-table field y)
  ;; F(.)(y) with a memo: the trees of a sum are subtrees of one another.
  (let ((memo (make-hash-table)))
    (define (F t)
      (let ((key (tree->string t)))
        (or (hash-ref memo key)
            (let ((value (field-apply field y (map F t))))
              (hash-set! memo key value)
              value))))
    F))

(define (weighted-tree-sum field y weight trees)
  ;; The sum over TREES of (WEIGHT t) * F(t)(y).
  (let ((F (differential-table field y)))
    (fold (lambda (t acc)
            (let ((w (weight t)))
              (if (and (exact? w) (zero? w))
                  acc
                  (map (lambda (a f) (+ a (* w f))) acc (F t)))))
          (make-list (field-dimension field) 0)
          trees)))

;;; B-series.

(define (b-series-sum field coefficient y h trees)
  "Y plus the sum over TREES of H^|t| / sigma(t) * (COEFFICIENT t) * F(t)(Y)."
  (map + y
       (weighted-tree-sum field y
                          (lambda (t)
                            (/ (* (expt h (tree-size t)) (coefficient t))
                               (tree-symmetry t)))
                          trees)))

(define (b-series field coefficient y h order)
  "The B-series of COEFFICIENT at Y with step H, summed over every tree
with at most ORDER nodes."
  (b-series-sum field coefficient y h (order-conditions order)))

(define (exact-flow-coefficient t)
  "1/gamma(T), the B-series coefficient of the exact flow."
  (/ 1 (tree-density t)))

(define (rk-coefficient tab)
  "The B-series coefficient map of the method TAB: its elementary weights."
  (lambda (t) (elementary-weight tab t)))

(define (rk-step tab field y h)
  "One step of size H of the explicit Runge-Kutta method TAB from Y."
  (unless (tableau-explicit? tab)
    (error "rk-step: the tableau is not explicit"))
  (let ((advance
         ;; y + h * sum_j w_j k_j over the stages computed so far.
         (lambda (weights stages)
           (fold (lambda (w k acc)
                   (if (and (exact? w) (zero? w))
                       acc
                       (map (lambda (a ki) (+ a (* h w ki))) acc k)))
                 y
                 weights
                 stages))))
    (let loop ((rows (tableau-a tab)) (stages '()))
      (if (null? rows)
          (advance (tableau-b tab) stages)
          (loop (cdr rows)
                (append stages
                        (list (field-apply field
                                           (advance (car rows) stages)
                                           '()))))))))

(define (repeat step y steps)
  (if (= steps 0) y (repeat step (step y) (- steps 1))))

(define (rk-integrate tab field y h steps)
  "STEPS steps of size H of the explicit method TAB from Y."
  (repeat (lambda (y) (rk-step tab field y h)) y steps))

;;; J-surfaces: the Taylor expansion of the exact flow over trees.
;;;
;;; The k-th derivative of the solution through y is
;;;
;;;   y^(k) = sum over trees t with k nodes of alpha(t) F(t)(y),
;;;
;;; alpha(t) = k!/(sigma(t) gamma(t)) being the number of monotone
;;; labellings of t, so y(h) = sum_k h^k/k! y^(k) is the B-series of the
;;; exact flow.

(define (solution-derivative field y k)
  "The K-th derivative at time 0 of the solution of y' = f(y) through Y."
  (unless (and (integer? k) (exact? k) (>= k 0))
    (error "solution-derivative: order must be a non-negative exact integer:" k))
  (if (= k 0)
      y
      (weighted-tree-sum field y tree-monotone-labellings (trees-of-size k))))

(define (solution-jet field y order)
  "The derivatives (y y' ... y^(ORDER)) of the solution through Y."
  (map (lambda (k) (solution-derivative field y k)) (iota (+ order 1))))

(define (taylor-flow field y h order)
  "The Taylor polynomial of degree ORDER in H of the solution through Y."
  (b-series field exact-flow-coefficient y h order))

(define (taylor-integrate field y h order steps)
  "STEPS steps of size H of the Taylor method of degree ORDER from Y."
  (repeat (lambda (y) (taylor-flow field y h order)) y steps))
