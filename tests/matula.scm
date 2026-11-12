(use-modules (srfi srfi-1) (srfi srfi-64)
             (tests support) (ghost tree) (ghost matula))

(test-begin "matula")

(test-equal "first primes" '(2 3 5 7 11 13 17 19 23 29) (map nth-prime (iota 10 1)))
(test-equal "1000th prime" 7919 (nth-prime 1000))
(test-equal "prime index" 1000 (prime-index 7919))
(test-equal "prime index of 2" 1 (prime-index 2))
(test-equal "composite has no index" #f (prime-index 7917))
(test-equal "1 has no index" #f (prime-index 1))
(test-assert "nth-prime and prime-index are inverse"
  (every (lambda (k) (= k (prime-index (nth-prime k)))) (iota 3000 1)))
(test-equal "factors of 1" '() (prime-factors 1))
(test-equal "factors of 360" '(2 2 2 3 3 5) (prime-factors 360))
(test-equal "factors of a prime" '(7919) (prime-factors 7919))
(test-assert "factors multiply back"
  (every (lambda (n) (= n (apply * (prime-factors n)))) (iota 2000 1)))

;; Known values.
(test-equal "single node" 1 (tree->matula '()))
(test-equal "chains" '(1 2 3 5 11 31 127 709)
  (map (lambda (n) (tree->matula (chain-tree n))) (iota 8 1)))
(test-equal "stars" '(1 2 4 8 16) (map (lambda (n) (tree->matula (star-tree n))) (iota 5 1)))
(test-equal "size-4 trees" '(8 6 7 5) (map tree->matula (trees-of-size 4)))
(test-equal "child order does not matter"
  (tree->matula '(() (())))
  (tree->matula '((()) ())))
(test-equal "decode 6" '((()) ()) (matula->tree 6))

;; Trees of size n are a set of a(n) distinct integers; the largest is OEIS
;; A005518.
(test-equal "largest Matula number by size"
  '(1 2 4 8 19 67 331 2221 19577 219613)
  (map (lambda (n) (apply max (map tree->matula (trees-of-size n))))
       (iota 10 1)))

(do ((n 1 (+ n 1))) ((> n 12))
  (test-assert (format #f "tree -> integer -> tree, size ~a" n)
    (every (lambda (t) (equal? t (matula->tree (tree->matula t))))
           (trees-of-size n))))

;; Every integer is a tree: 1..5000 decode to distinct trees and encode back.
(test-assert "integer -> tree -> integer for 1..5000"
  (every (lambda (m) (= m (tree->matula (matula->tree m)))) (iota 5000 1)))
(test-assert "decoded trees are canonical"
  (every (lambda (m) (tree-canonical? (matula->tree m))) (iota 500 1)))

;; The integers whose trees have n nodes are exactly the codes of
;; trees-of-size n (checked where the largest code is small).
(do ((n 1 (+ n 1))) ((> n 8))
  (let ((largest (apply max (map tree->matula (trees-of-size n)))))
    (test-equal (format #f "integers decoding to size ~a" n)
      (sort (map tree->matula (trees-of-size n)) <)
      (filter (lambda (m) (= n (tree-size (matula->tree m))))
              (iota largest 1)))))

;; Shapes.
(test-equal "shape of the single node is a scalar" '() (matula-shape '()))
(test-equal "shape of [[.],.]" '(2 3) (matula-shape '((()) ())))
(test-equal "one axis per root child"
  (map length (trees-of-size 7))
  (map (lambda (t) (length (matula-shape t))) (trees-of-size 7)))
(test-assert "element count is the product of the axes"
  (every (lambda (t) (= (matula-shape-elements t) (apply * (matula-shape t))))
         (trees-of-size 9)))
(test-equal "shapes are distinct across trees of size 9"
  (length (trees-of-size 9))
  (length (delete-duplicates (map matula-shape (trees-of-size 9)))))

(test-finish "matula")
