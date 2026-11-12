;;; (ghost tree) - unlabelled rooted trees in canonical form
;;;
;;; A tree is the list of its root's subtrees, so the single node is '() and
;;; the chain on three nodes is '((())).  The canonical form lists the
;;; subtrees in non-increasing order under `tree-compare', recursively; two
;;; trees are isomorphic exactly when their canonical forms are `equal?'.

(define-module (ghost tree)
  #:use-module (srfi srfi-1)
  #:use-module (ghost a000081)
  #:export (tree?
            tree-size
            tree-height
            tree-compare
            tree<?
            tree-canonical
            tree-canonical?
            tree-symmetry
            tree-density
            tree->string
            trees-of-size
            chain-tree
            star-tree
            rooted-parent-map?
            parent-map->tree
            tree->parent-map
            rooted-parent-maps
            tree->partition
            partition-fibres
            partition-fibre-size))

(define (tree? x)
  "True if X is a nested proper list of empty lists, canonical or not."
  (and (list? x) (every tree? x)))

(define (tree-size t)
  "The number of nodes |t|."
  (fold (lambda (child sum) (+ sum (tree-size child))) 1 t))

(define (tree-height t)
  "The number of edges on the longest path from the root to a leaf."
  (if (null? t)
      0
      (+ 1 (fold (lambda (child h) (max h (tree-height child))) 0 t))))

(define (compare-canonical a sa b sb)
  ;; A and B canonical with sizes SA and SB.
  (cond ((< sa sb) -1)
        ((> sa sb) 1)
        (else
         (let loop ((as a) (bs b))
           (cond ((null? as) 0)        ; equal sizes, so BS is null too
                 ((null? bs) 1)
                 (else
                  (let ((c (compare-canonical (car as) (tree-size (car as))
                                              (car bs) (tree-size (car bs)))))
                    (if (= c 0)
                        (loop (cdr as) (cdr bs))
                        c))))))))

(define (tree-compare a b)
  "Total order on canonical trees: -1, 0 or 1.  Smaller trees come first;
trees of equal size compare lexicographically on their subtree lists."
  (compare-canonical a (tree-size a) b (tree-size b)))

(define (tree<? a b)
  (< (tree-compare a b) 0))

(define (tree-canonical t)
  "The canonical form of the nested list T."
  (unless (tree? t)
    (error "tree-canonical: not a tree:" t))
  (let canon ((t t))
    (sort (map canon t) (lambda (a b) (> (tree-compare a b) 0)))))

(define (tree-canonical? t)
  (and (tree? t) (equal? t (tree-canonical t))))

(define (factorial n)
  (let loop ((i 2) (acc 1))
    (if (> i n) acc (loop (+ i 1) (* acc i)))))

(define (runs canonical-children)
  "Group equal neighbours: a list of (subtree . multiplicity)."
  (let loop ((cs canonical-children) (acc '()))
    (cond ((null? cs) (reverse acc))
          ((and (pair? acc) (equal? (caar acc) (car cs)))
           (loop (cdr cs) (cons (cons (caar acc) (+ 1 (cdar acc))) (cdr acc))))
          (else (loop (cdr cs) (cons (cons (car cs) 1) acc))))))

(define (tree-symmetry t)
  "sigma(t), the order of the automorphism group of T: the product over
the distinct root subtrees c with multiplicity m of m! * sigma(c)^m."
  (fold (lambda (run acc)
          (* acc
             (factorial (cdr run))
             (expt (tree-symmetry (car run)) (cdr run))))
        1
        (runs (tree-canonical t))))

(define (tree-density t)
  "gamma(t) = |t| * product of gamma over the root subtrees."
  (fold (lambda (child acc) (* acc (tree-density child)))
        (tree-size t)
        t))

(define (tree->string t)
  "Balanced-parenthesis form, e.g. \"(()(()))\"."
  (string-append "(" (string-concatenate (map tree->string t)) ")"))

;;; Enumeration.
;;;
;;; A tree of size n is a multiset of trees of total size n-1.  Each multiset
;;; is produced once, as a non-increasing sequence, and the sequences come
;;; out in increasing lexicographic order, so `trees-of-size' is sorted by
;;; `tree-compare' and position within the list agrees with that order.

(define size-cache (make-hash-table))   ; n -> vector of trees, ascending

(define (trees-vector n)
  (or (hashv-ref size-cache n)
      (let ((v (list->vector (generate n))))
        (hashv-set! size-cache n v)
        v)))

(define (forests m max-size max-index)
  "Forests of total size M, as non-increasing lists of trees no larger than
tree number MAX-INDEX of size MAX-SIZE, in increasing lexicographic order."
  (if (= m 0)
      '(())
      (let size-loop ((s 1) (acc '()))
        (if (> s (min m max-size))
            (reverse acc)
            (let* ((v (trees-vector s))
                   (last (if (= s max-size)
                             max-index
                             (- (vector-length v) 1))))
              (let index-loop ((i 0) (acc acc))
                (if (> i last)
                    (size-loop (+ s 1) acc)
                    (let ((first (vector-ref v i)))
                      (index-loop
                       (+ i 1)
                       (fold (lambda (rest acc) (cons (cons first rest) acc))
                             acc
                             (forests (- m s) s i)))))))))))

(define (generate n)
  (if (= n 1)
      '(())
      (forests (- n 1)
               (- n 1)
               (- (vector-length (trees-vector (- n 1))) 1))))

(define (trees-of-size n)
  "All unlabelled rooted trees with N nodes, canonical, without repetition,
in increasing `tree-compare' order.  There are (a000081 N) of them."
  (unless (and (integer? n) (exact? n) (>= n 0))
    (error "trees-of-size: size must be a non-negative exact integer:" n))
  (if (= n 0)
      '()
      (vector->list (trees-vector n))))

(define (chain-tree n)
  "The path on N >= 1 nodes rooted at an end."
  (if (<= n 1) '() (list (chain-tree (- n 1)))))

(define (star-tree n)
  "The root with N-1 leaf children."
  (make-list (- n 1) '()))

;;; Labelled rooted trees as endofunctions.
;;;
;;; A map f: {0..n-1} -> {0..n-1}, given as a vector, is a labelled rooted
;;; tree when it has exactly one fixed point (the root) and its functional
;;; graph is connected, i.e. every node reaches the root.  There are n^(n-1)
;;; such maps.  They correspond to labelled trees, not unlabelled ones; the
;;; unlabelled trees are the orbits under relabelling, and the orbit of a
;;; tree t has n!/sigma(t) elements.

(define (rooted-parent-map? f)
  (let* ((n (vector-length f))
         (fixed (filter (lambda (i) (= i (vector-ref f i))) (iota n))))
    (and (= 1 (length fixed))
         (let ((root (car fixed)))
           (every (lambda (start)
                    ;; A walk of n steps either reaches the root or is
                    ;; trapped on a cycle that avoids it.
                    (let walk ((i start) (steps 0))
                      (cond ((= i root) #t)
                            ((= steps n) #f)
                            (else (walk (vector-ref f i) (+ steps 1))))))
                  (iota n))))))

(define (parent-map->tree f)
  "The canonical unlabelled tree of the rooted parent map F."
  (unless (rooted-parent-map? f)
    (error "parent-map->tree: not a rooted parent map:" f))
  (let* ((n (vector-length f))
         (children (make-vector n '()))
         (root #f))
    (let loop ((i (- n 1)))
      (when (>= i 0)
        (let ((p (vector-ref f i)))
          (if (= p i)
              (set! root i)
              (vector-set! children p (cons i (vector-ref children p)))))
        (loop (- i 1))))
    (tree-canonical
     (let build ((i root))
       (map build (vector-ref children i))))))

(define (tree->parent-map t)
  "A rooted parent map for T, with nodes numbered in preorder (root 0)."
  (let ((f (make-vector (tree-size t) 0))
        (next 0))
    (let visit ((t t) (parent 0))
      (let ((me next))
        (set! next (+ next 1))
        (vector-set! f me (if (= me 0) 0 parent))
        (for-each (lambda (child) (visit child me)) t)))
    f))

(define (rooted-parent-maps n)
  "All rooted parent maps on N points, by brute force over the N^N maps."
  (let ((f (make-vector n 0))
        (acc '()))
    (let fill ((i 0))
      (if (= i n)
          (when (rooted-parent-map? f)
            (set! acc (cons (vector-copy f) acc)))
          (let loop ((v 0))
            (when (< v n)
              (vector-set! f i v)
              (fill (+ i 1))
              (loop (+ v 1))))))
    (reverse acc)))

;;; Trees and partitions.
;;;
;;; Sending a tree to the sizes of its root subtrees maps the trees of size n
;;; onto the partitions of n-1.  It is surjective but not injective.

(define (tree->partition t)
  "The sizes of the root subtrees of T, in non-increasing order."
  (sort (map tree-size t) >))

(define (partition-fibres n)
  "An alist from each partition of N-1 to the number of trees of size N that
map to it under `tree->partition'."
  (let ((table (make-hash-table)))
    (for-each (lambda (t)
                (let ((p (tree->partition t)))
                  (hash-set! table p (+ 1 (hash-ref table p 0)))))
              (trees-of-size n))
    (sort (hash-map->list cons table)
          (lambda (a b) (string<? (object->string (car a))
                                  (object->string (car b)))))))

(define (binomial n k)
  (let loop ((i 1) (acc 1))
    (if (> i k) acc (loop (+ i 1) (/ (* acc (+ (- n k) i)) i)))))

(define (partition-fibre-size partition)
  "The number of trees whose root subtree sizes are PARTITION, in closed
form: for each part size s occurring m times, choose a multiset of m trees
of size s, C(a(s)+m-1, m) ways."
  (let loop ((parts (sort partition >)) (acc 1))
    (if (null? parts)
        acc
        (let* ((s (car parts))
               (m (length (take-while (lambda (x) (= x s)) parts))))
          (loop (drop parts m)
                (* acc (binomial (+ (a000081 s) m -1) m)))))))
