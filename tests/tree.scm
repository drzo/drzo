(use-modules (srfi srfi-1) (srfi srfi-64)
             (tests support) (ghost a000081) (ghost tree))

(test-begin "tree")

(define (factorial n) (if (< n 2) 1 (* n (factorial (- n 1)))))

(define (strictly-increasing? trees)
  (or (null? trees)
      (null? (cdr trees))
      (and (tree<? (car trees) (cadr trees))
           (strictly-increasing? (cdr trees)))))

;;; Representation.

(test-assert "single node" (tree? '()))
(test-assert "not a tree" (not (tree? '(() x))))
(test-equal "size" 5 (tree-size '((()) () ())))
(test-equal "height of chain" 4 (tree-height (chain-tree 5)))
(test-equal "height of star" 1 (tree-height (star-tree 5)))
(test-equal "height of single node" 0 (tree-height '()))
(test-equal "canonical form ignores child order"
  (tree-canonical '(() (()) ((()) ())))
  (tree-canonical '((() (())) () (()))))
(test-assert "different trees have different canonical forms"
  (not (equal? (tree-canonical '((()) (())))
               (tree-canonical '((() ()) ())))))
(test-assert "canonical? accepts canonical" (tree-canonical? '((()) ())))
(test-assert "canonical? rejects unsorted" (not (tree-canonical? '(() (())))))
(test-equal "string form" "((())())" (tree->string '((()) ())))
(test-error "canonical rejects non-trees" #t (tree-canonical '(1 2)))

;;; Enumeration: a(n) trees of size n, canonical, sorted, no duplicates.

(test-equal "no trees of size 0" '() (trees-of-size 0))
(test-equal "trees of size 4"
  '((() () ()) ((()) ()) ((() ())) (((()))))
  (trees-of-size 4))

(do ((n 1 (+ n 1))) ((> n 18))
  (let ((trees (trees-of-size n)))
    (test-equal (format #f "count of size ~a is a(~a)" n n)
      (a000081 n)
      (length trees))
    ;; Strictly increasing under a total order implies pairwise distinct.
    (test-assert (format #f "size ~a strictly increasing, so no duplicates" n)
      (strictly-increasing? trees))))

(do ((n 1 (+ n 1))) ((> n 12))
  (let ((trees (trees-of-size n)))
    (test-assert (format #f "size ~a: every tree has ~a nodes" n n)
      (every (lambda (t) (= n (tree-size t))) trees))
    (test-assert (format #f "size ~a: every tree is canonical" n)
      (every tree-canonical? trees))))

(test-assert "tree-compare is antisymmetric on size 7"
  (let ((trees (trees-of-size 7)))
    (every (lambda (a)
             (every (lambda (b) (= (tree-compare a b) (- (tree-compare b a))))
                    trees))
           trees)))

;;; Invariants.

(test-equal "sigma of star" (factorial 5) (tree-symmetry (star-tree 6)))
(test-equal "sigma of chain" 1 (tree-symmetry (chain-tree 6)))
(test-equal "sigma of two cherries" 8 (tree-symmetry '((() ()) (() ()))))
(test-equal "sigma does not depend on child order"
  (tree-symmetry '((()) () (())))
  (tree-symmetry '(() (()) (()))))
(test-equal "gamma of chain" (factorial 6) (tree-density (chain-tree 6)))
(test-equal "gamma of star" 6 (tree-density (star-tree 6)))
(test-equal "gamma of [[.],.]" 8 (tree-density '((()) ())))

;; Burnside: the labellings of all trees of size n number n^(n-1).
(do ((n 1 (+ n 1))) ((> n 14))
  (test-equal (format #f "sum of n!/sigma is n^(n-1), n = ~a" n)
    (expt n (- n 1))
    (fold (lambda (t sum) (+ sum (/ (factorial n) (tree-symmetry t))))
          0
          (trees-of-size n))))

;; sum over |t| = n of n!/(sigma gamma) = (n-1)!
(do ((n 1 (+ n 1))) ((> n 14))
  (test-equal (format #f "sum of n!/(sigma gamma) is (n-1)!, n = ~a" n)
    (factorial (- n 1))
    (fold (lambda (t sum)
            (+ sum (/ (factorial n) (* (tree-symmetry t) (tree-density t)))))
          0
          (trees-of-size n))))

;;; Endofunctions: brute force over all n^n maps.

(test-assert "root loop only" (rooted-parent-map? #(0)))
(test-assert "chain" (rooted-parent-map? #(0 0 1)))
(test-assert "two fixed points" (not (rooted-parent-map? #(0 1 0))))
(test-assert "no fixed point" (not (rooted-parent-map? #(1 0))))
(test-assert "fixed point plus a separate 2-cycle"
  (not (rooted-parent-map? #(0 2 1))))
(test-equal "parent map to tree" '((()) ()) (parent-map->tree #(2 2 2 0)))
(test-error "non-tree map rejected" #t (parent-map->tree #(0 2 1)))

(do ((n 1 (+ n 1))) ((> n 6))
  (let* ((maps (rooted-parent-maps n))
         (counts (make-hash-table)))
    (for-each (lambda (f)
                (let ((t (parent-map->tree f)))
                  (hash-set! counts t (+ 1 (hash-ref counts t 0)))))
              maps)
    (test-equal (format #f "n = ~a: n^(n-1) rooted parent maps" n)
      (expt n (- n 1))
      (length maps))
    (test-equal (format #f "n = ~a: a(n) orbits under relabelling" n)
      (a000081 n)
      (hash-count (const #t) counts))
    (test-assert (format #f "n = ~a: the orbits are exactly trees-of-size" n)
      (every (lambda (t) (hash-ref counts t)) (trees-of-size n)))
    ;; Orbit-stabiliser: this checks sigma against an independent count.
    (test-assert (format #f "n = ~a: each orbit has n!/sigma maps" n)
      (every (lambda (t)
               (= (hash-ref counts t 0) (/ (factorial n) (tree-symmetry t))))
             (trees-of-size n)))))

(do ((n 1 (+ n 1))) ((> n 9))
  (test-assert (format #f "tree -> parent map -> tree, size ~a" n)
    (every (lambda (t)
             (let ((f (tree->parent-map t)))
               (and (= 0 (vector-ref f 0))
                    (increasing-parent-map? f)
                    (equal? t (parent-map->tree f)))))
           (trees-of-size n))))

;;; Increasing parent maps: (n-1)! of them, alpha(t) with each shape t.
;;; This counts monotone labellings directly, without sigma or gamma.

(test-assert "increasing: chain" (increasing-parent-map? #(0 0 1 2)))
(test-assert "increasing: star" (increasing-parent-map? #(0 0 0 0)))
(test-assert "not increasing: parent after child"
  (not (increasing-parent-map? #(0 2 0))))
(test-assert "not increasing: root is not node 0"
  (not (increasing-parent-map? #(1 1))))
(test-assert "not increasing: empty" (not (increasing-parent-map? #())))
(test-equal "alpha of the chain" 1 (tree-monotone-labellings (chain-tree 6)))
(test-equal "alpha of the star" 1 (tree-monotone-labellings (star-tree 6)))
(test-equal "alpha of [[.],.]" 3 (tree-monotone-labellings '((()) ())))
(test-error "no increasing maps on 0 points" #t (increasing-parent-maps 0))

(do ((n 1 (+ n 1))) ((> n 8))
  (let ((maps (increasing-parent-maps n))
        (counts (make-hash-table)))
    (for-each (lambda (f)
                (let ((t (parent-map->tree f)))
                  (hash-set! counts t (+ 1 (hash-ref counts t 0)))))
              maps)
    (test-equal (format #f "n = ~a: (n-1)! increasing parent maps" n)
      (factorial (- n 1))
      (length maps))
    (test-assert (format #f "n = ~a: every increasing map is a rooted parent map" n)
      (every rooted-parent-map? maps))
    (test-assert (format #f "n = ~a: alpha(t) increasing maps have shape t" n)
      (every (lambda (t)
               (= (hash-ref counts t 0) (tree-monotone-labellings t)))
             (trees-of-size n)))))

;;; Partitions: a surjection onto the partitions of n-1, not a bijection.

(define partition-numbers '(1 1 2 3 5 7 11 15 22 30 42 56))   ; p(0)..p(11)

(test-equal "partition of a tree" '(3 1 1) (tree->partition '(() ((())) ())))

(do ((n 1 (+ n 1))) ((> n 12))
  (let ((fibres (partition-fibres n)))
    (test-equal (format #f "n = ~a: every partition of n-1 is hit" n)
      (list-ref partition-numbers (- n 1))
      (length fibres))
    (test-assert (format #f "n = ~a: keys are partitions of n-1" n)
      (every (lambda (entry)
               (and (= (- n 1) (apply + (car entry)))
                    (apply >= (if (null? (car entry)) '(0) (car entry)))))
             fibres))
    (test-equal (format #f "n = ~a: fibre sizes sum to a(n)" n)
      (a000081 n)
      (apply + (map cdr fibres)))
    (test-assert (format #f "n = ~a: fibre sizes match the closed form" n)
      (every (lambda (entry)
               (= (cdr entry) (partition-fibre-size (car entry))))
             fibres))))

(test-assert "the map is not injective: a(5) = 9 > p(4) = 5"
  (> (a000081 5) (length (partition-fibres 5))))

(test-finish "tree")
