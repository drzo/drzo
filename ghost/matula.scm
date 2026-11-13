;;; (ghost matula) - Matula-Goebel numbers of rooted trees
;;;
;;; M(single node) = 1 and M(t) = product over the root subtrees c of
;;; p_{M(c)}, where p_k is the k-th prime (p_1 = 2).  By unique
;;; factorisation this is a bijection between unlabelled rooted trees and
;;; the positive integers.

(define-module (ghost matula)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:use-module (ghost tree)
  #:export (nth-prime
            prime-index
            prime-factors
            tree->matula
            matula->tree
            matula-shape
            matula-shape-elements
            matula-element-limit
            tree->matula/bounded
            matula-shape/bounded
            tree-product
            tree-plant))

;;; Primes, from a sieve that is rebuilt at twice the size when it runs out.

(define sieve-limit 0)
(define primes (vector))                 ; primes below sieve-limit, ascending

(define (build-sieve! limit)
  (let ((composite (make-bytevector (+ limit 1) 0)))
    (let loop ((i 2))
      (when (<= (* i i) limit)
        (when (= 0 (bytevector-u8-ref composite i))
          (let mark ((j (* i i)))
            (when (<= j limit)
              (bytevector-u8-set! composite j 1)
              (mark (+ j i)))))
        (loop (+ i 1))))
    (let collect ((i limit) (acc '()))
      (if (< i 2)
          (begin
            (set! primes (list->vector acc))
            (set! sieve-limit limit))
          (collect (- i 1)
                   (if (= 0 (bytevector-u8-ref composite i))
                       (cons i acc)
                       acc))))))

(define (ensure-sieve-to! n)
  (when (< sieve-limit n)
    (build-sieve! (max n (* 2 sieve-limit) 1024))))

(define (nth-prime k)
  "The K-th prime, with (nth-prime 1) = 2."
  (unless (and (integer? k) (exact? k) (>= k 1))
    (error "nth-prime: index must be a positive exact integer:" k))
  (let loop ()
    (if (<= k (vector-length primes))
        (vector-ref primes (- k 1))
        (begin
          (build-sieve! (max 1024 (* 2 sieve-limit)))
          (loop)))))

(define (prime-index p)
  "K such that P is the K-th prime, or #f if P is not prime."
  (and (integer? p) (exact? p) (>= p 2)
       (begin
         (ensure-sieve-to! p)
         (let search ((lo 0) (hi (- (vector-length primes) 1)))
           (and (<= lo hi)
                (let* ((mid (quotient (+ lo hi) 2))
                       (q (vector-ref primes mid)))
                  (cond ((= q p) (+ mid 1))
                        ((< q p) (search (+ mid 1) hi))
                        (else (search lo (- mid 1))))))))))

(define (prime-factors n)
  "The prime factors of N >= 1 with multiplicity, in non-decreasing order."
  (unless (and (integer? n) (exact? n) (>= n 1))
    (error "prime-factors: argument must be a positive exact integer:" n))
  (let loop ((n n) (d 2) (acc '()))
    (cond ((= n 1) (reverse acc))
          ((> (* d d) n) (reverse (cons n acc)))
          ((= 0 (remainder n d)) (loop (quotient n d) d (cons d acc)))
          (else (loop n (if (= d 2) 3 (+ d 2)) acc)))))

(define (tree->matula t)
  "The Matula-Goebel number of the tree T."
  (fold (lambda (child acc) (* acc (nth-prime (tree->matula child))))
        1
        t))

(define (matula->tree n)
  "The canonical tree whose Matula-Goebel number is N."
  (unless (and (integer? n) (exact? n) (>= n 1))
    (error "matula->tree: argument must be a positive exact integer:" n))
  (tree-canonical
   (let decode ((n n))
     (map (lambda (p) (decode (prime-index p)))
          (prime-factors n)))))

;;; Tensor shapes.
;;;
;;; The shape of a tree has one axis per root subtree c, of length
;;; p_{M(c)}, listed in non-decreasing order.  It is the prime factorisation
;;; of M(t), so distinct trees have distinct shapes and the number of
;;; elements is M(t) itself: a buffer of N elements has exactly one tree
;;; shape, the one whose Matula number is N.
;;;
;;; Lengths grow quickly with tree size (the chain on 12 nodes has a single
;;; axis of length 9737333), and the numbers themselves soon cannot be
;;; computed at all.  `matula-shape' is exact and unbounded.  Code that
;;; allocates, or that must not hang on a large tree, uses the bounded
;;; procedures below, which answer #f above a limit without doing the work.

(define (matula-shape t)
  "The axis lengths of T's tensor shape; '() (a scalar) for the single node."
  (prime-factors (tree->matula t)))

(define (matula-shape-elements t)
  "The number of elements of a tensor of shape (matula-shape T)."
  (tree->matula t))

(define matula-element-limit
  ;; 2^20 elements: every tree with at most 10 nodes fits (the largest
  ;; Matula number among them is 219613), and no tree with a single axis
  ;; longer than 1048573.
  (make-parameter (expt 2 20)))

(define (primes-up-to limit)
  ;; The number of primes <= LIMIT.
  (ensure-sieve-to! limit)
  (let search ((lo 0) (hi (vector-length primes)))
    ;; The first index whose prime exceeds LIMIT.
    (if (= lo hi)
        lo
        (let ((mid (quotient (+ lo hi) 2)))
          (if (<= (vector-ref primes mid) limit)
              (search (+ mid 1) hi)
              (search lo mid))))))

(define* (tree->matula/bounded t #:optional (limit (matula-element-limit)))
  "The Matula-Goebel number of T if it is at most LIMIT, and otherwise #f.
No prime above LIMIT is ever computed."
  (unless (and (integer? limit) (exact? limit) (>= limit 1))
    (error "tree->matula/bounded: limit must be a positive exact integer:" limit))
  (let ((count (primes-up-to limit)))
    (let number ((t t))
      (let loop ((children t) (product 1))
        (if (null? children)
            product
            (let ((m (number (car children))))
              ;; p_m <= LIMIT exactly when m <= COUNT.
              (and m
                   (<= m count)
                   (let ((product (* product (nth-prime m))))
                     (and (<= product limit)
                          (loop (cdr children) product))))))))))

(define* (matula-shape/bounded t #:optional (limit (matula-element-limit)))
  "The tensor shape of T if it has at most LIMIT elements, and otherwise #f."
  (let ((m (tree->matula/bounded t limit)))
    (and m (prime-factors m))))

;;; The two operations that build every tree, and what they do to shapes.

(define (tree-product a b)
  "The tree whose root subtrees are those of A together with those of B.
Matula numbers multiply and shapes concatenate."
  (tree-canonical (append a b)))

(define (tree-plant t)
  "T planted on a new root.  Its Matula number is the M(T)-th prime, and
its shape is that single axis."
  (list t))
