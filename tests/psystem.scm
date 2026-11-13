(use-modules (srfi srfi-1) (srfi srfi-64)
             (tests support) (ghost a000081) (ghost tree) (ghost psystem))

(test-begin "psystem")

;;; Schedulers for the tests.

(define (no-conflict label rules)
  (error "the scheduler was consulted, but no rules compete here:" label))

(define (scripted . choices)
  "Answer with CHOICES in turn; fail if asked more often."
  (lambda (label rules)
    (when (null? choices)
      (error "the scripted scheduler ran out of choices in region:" label))
    (let ((choice (car choices)))
      (set! choices (cdr choices))
      choice)))

;;; Multisets.

(test-equal "multiset is sorted and counted"
  '((a . 2) (b . 1) (c . 3))
  (list->multiset '(c a b c a c)))
(test-equal "multiset count" 3 (multiset-count (list->multiset '(c a c c)) 'c))
(test-equal "multiset count of an absent object" 0
  (multiset-count (list->multiset '(a)) 'z))
(test-equal "multiset size" 6 (multiset-size (list->multiset '(c a b c a c))))
(test-error "multiset of non-symbols" #t (list->multiset '(1 2)))

;;; Paun's system generating the squares.  Membrane 3 holds a f.  While
;;; a -> a b is chosen, f doubles; a -> b (dissolve) ends that phase after
;;; n rounds with b^(n+1) f^(2^(n+1)) released into membrane 2.  There each
;;; b becomes d, every d emits an e per step, and f f -> f halves the f's
;;; until one is left to dissolve membrane 2, n+1 steps of emission later.
;;; The skin ends with (n+1)^2 copies of e.

(define (squares-system)
  (let* ((halve (make-rule '(f f) '(f)))
         (finish (make-rule '(f) '() #:dissolve? #t #:yields-to (list halve))))
    (make-psystem
     '(1 (2 (3)))
     '((3 a f))
     `((2 ,(make-rule '(b) '(d))
          ,(make-rule '(d) '(d e))
          ,halve
          ,finish)
       (3 ,(make-rule '(a) '(a b))
          ,(make-rule '(a) '(b) #:dissolve? #t)
          ,(make-rule '(f) '(f f)))))))

(define (grow-then-dissolve n)
  (apply scripted (append (make-list n 0) '(1))))

(do ((n 0 (+ n 1))) ((> n 6))
  (let ((end (psystem-run (squares-system) (grow-then-dissolve n))))
    (test-assert (format #f "squares, n = ~a: halts" n) (psystem-halted? end))
    (test-equal (format #f "squares, n = ~a: (n+1)^2 copies of e in the skin" n)
      (* (+ n 1) (+ n 1))
      (psystem-count end 1 'e))
    (test-equal (format #f "squares, n = ~a: the skin holds only d^(n+1) e^((n+1)^2)" n)
      `((d . ,(+ n 1)) (e . ,(* (+ n 1) (+ n 1))))
      (psystem-contents end 1))
    (test-equal (format #f "squares, n = ~a: takes 2n+3 steps" n)
      (+ (* 2 n) 3)
      (psystem-steps end))
    (test-equal (format #f "squares, n = ~a: both inner membranes dissolved" n)
      '()
      (psystem-structure end))))

(test-equal "squares, n = 2: configuration by configuration"
  '((1 () (2 () (3 ((a . 1) (f . 1)))))
    (1 () (2 () (3 ((a . 1) (b . 1) (f . 2)))))
    (1 () (2 () (3 ((a . 1) (b . 2) (f . 4)))))
    (1 () (2 ((b . 3) (f . 8))))
    (1 () (2 ((d . 3) (f . 4))))
    (1 () (2 ((d . 3) (e . 3) (f . 2))))
    (1 () (2 ((d . 3) (e . 6) (f . 1))))
    (1 ((d . 3) (e . 9))))
  (map car (psystem-trace (squares-system) (grow-then-dissolve 2))))

;; Under a seeded scheduler the output is still a square, whatever the seed.
(define (seeded-run seed)
  (psystem-run (squares-system) (make-seeded-scheduler seed) 500))

(define seeds (iota 40 1))

(test-assert "squares: every seeded run halts"
  (every (lambda (seed) (psystem-halted? (seeded-run seed))) seeds))
(test-assert "squares: every seeded run ends with m^2 copies of e and m of d"
  (every (lambda (seed)
           (let* ((end (seeded-run seed))
                  (m (psystem-count end 1 'd)))
             (and (>= m 1)
                  (= (* m m) (psystem-count end 1 'e))
                  (= (+ (* 2 m) 1) (psystem-steps end)))))
         seeds))
(test-assert "squares: different seeds reach different squares"
  (> (length (delete-duplicates
              (map (lambda (seed) (psystem-count (seeded-run seed) 1 'e)) seeds)))
     1))
(test-assert "the same seed gives the same computation, step for step"
  (every (lambda (seed)
           (equal? (psystem-trace (squares-system) (make-seeded-scheduler seed) 500)
                   (psystem-trace (squares-system) (make-seeded-scheduler seed) 500)))
         seeds))

;;; Maximal parallelism.

(define (single-region contents . rules)
  (make-psystem '(1) `((1 . ,contents)) `((1 . ,rules))))

(test-equal "a a -> b on a^5: twice, leaving one a"
  '((a . 1) (b . 2))
  (psystem-contents
   (psystem-step (single-region '(a a a a a) (make-rule '(a a) '(b))) no-conflict)
   1))

(let ((doubling (single-region '(a) (make-rule '(a) '(a a)))))
  (test-equal "a -> a a doubles every step, with nothing for the scheduler to decide"
    '(1 2 4 8 16 32 64 128 256 512 1024)
    (map (lambda (configuration) (multiset-count (cadr (car configuration)) 'a))
         (psystem-trace doubling no-conflict 10)))
  (test-equal "a system that never halts stops at the step limit" 10
    (psystem-steps (psystem-run doubling no-conflict 10))))

;; a a -> b and a a a -> c compete on a^7.  A maximal choice leaves fewer
;; than two a's, so (b, c, a left) is (3,0,1), (2,1,0) or (0,2,1).
(define (compete seed)
  (let ((end (psystem-step
              (single-region '(a a a a a a a)
                             (make-rule '(a a) '(b))
                             (make-rule '(a a a) '(c)))
              (make-seeded-scheduler seed))))
    (list (psystem-count end 1 'b) (psystem-count end 1 'c) (psystem-count end 1 'a))))

(test-assert "competing rules: every outcome is maximal"
  (every (lambda (seed) (member (compete seed) '((3 0 1) (2 1 0) (0 2 1)))) seeds))
(test-assert "competing rules: more than one outcome occurs"
  (> (length (delete-duplicates (map compete seeds))) 1))
(test-equal "competing rules: the first-rule scheduler always takes a a -> b"
  '((a . 1) (b . 3))
  (psystem-contents
   (psystem-step (single-region '(a a a a a a a)
                                (make-rule '(a a) '(b))
                                (make-rule '(a a a) '(c)))
                 (first-rule-scheduler))
   1))
(test-equal "competing rules: a script decides"
  '((b . 2) (c . 1))
  (psystem-contents
   (psystem-step (single-region '(a a a a a a a)
                                (make-rule '(a a) '(b))
                                (make-rule '(a a a) '(c)))
                 (scripted 1 0 0))
   1))
(test-error "a scheduler answering out of range is an error" #t
  (psystem-step (single-region '(a) (make-rule '(a) '(b)) (make-rule '(a) '(c)))
                (lambda (label rules) 2)))

;; Products arrive at the end of the step: b made from a is not used in the
;; step that makes it.
(test-equal "products are not consumed in the step that makes them"
  '(((a . 2)) ((b . 2)) ((c . 2)))
  (map (lambda (configuration) (cadr (car configuration)))
       (psystem-trace (single-region '(a a)
                                     (make-rule '(a) '(b))
                                     (make-rule '(b) '(c)))
                      no-conflict)))

;;; Priority is strong and is a partial order.

;; On f^3, f f -> f is applicable, so f -> g is barred for the whole step
;; even though one f is left over.  A weak priority would give f g at once.
(let* ((halve (make-rule '(f f) '(f)))
       (convert (make-rule '(f) '(g) #:yields-to (list halve))))
  (test-equal "strong priority: the leftover f waits"
    '(((f . 3)) ((f . 2)) ((f . 1)) ((g . 1)))
    (map (lambda (configuration) (cadr (car configuration)))
         (psystem-trace (single-region '(f f f) halve convert) no-conflict))))

;; r-c yields to r-a only; r-b is unrelated to both and is never barred.
(define (priority-run contents)
  (let* ((r-a (make-rule '(a) '(x)))
         (r-b (make-rule '(b) '(y)))
         (r-c (make-rule '(c) '(z) #:yields-to (list r-a))))
    (psystem-contents (psystem-step (single-region contents r-a r-b r-c) no-conflict)
                      1)))

(test-equal "priority: c waits while a is present, b does not"
  '((c . 1) (x . 1) (y . 1))
  (priority-run '(a b c)))
(test-equal "priority: without a, c fires"
  '((y . 1) (z . 1))
  (priority-run '(b c)))

;; Transitivity: r3 < r2 < r1; with r2 inapplicable, r1 still bars r3.
(let* ((r1 (make-rule '(a) '(x)))
       (r2 (make-rule '(b) '(y) #:yields-to (list r1)))
       (r3 (make-rule '(c) '(z) #:yields-to (list r2))))
  (test-equal "priority is transitive"
    '((c . 1) (x . 1))
    (psystem-contents (psystem-step (single-region '(a c) r1 r2 r3) no-conflict) 1)))

;;; Targets.

(let ((relay (make-psystem
              '(1 (2))
              '((1 a))
              `((1 ,(make-rule '(a) '((a in 2)))
                   ,(make-rule '(b) '((c out))))
                (2 ,(make-rule '(a) '((b out))))))))
  (test-equal "in, out, and out of the skin into the environment"
    '(((1 ((a . 1)) (2 ())) . ())
      ((1 () (2 ((a . 1)))) . ())
      ((1 ((b . 1)) (2 ())) . ())
      ((1 () (2 ())) . ((c . 1))))
    (psystem-trace relay no-conflict))
  (test-equal "environment of the halted system"
    '((c . 1))
    (psystem-environment (psystem-run relay no-conflict))))

(test-equal "a rule with several targets"
  '((1 ((x . 2)) (2 ((y . 1)))) . ((z . 3)))
  (cadr (psystem-trace
         (make-psystem '(1 (2)) '((1 a))
                       `((1 ,(make-rule '(a) '(x (y in 2) (z out) x (z out) (z out))))))
         no-conflict)))

(test-assert "a rule sending into a membrane that is not directly inside never applies"
  (psystem-halted?
   (make-psystem '(1 (2 (3))) '((1 a)) `((1 ,(make-rule '(a) '((a in 3))))))))

;;; Dissolution.

(test-equal "a dissolved membrane leaves its objects and inner membranes to its parent"
  '(1 ((p . 1) (q . 1) (y . 1)) (3 ((r . 1))))
  (psystem-membranes
   (psystem-step
    (make-psystem '(1 (2 (3))) '((1 p) (2 q x) (3 r))
                  `((2 ,(make-rule '(x) '(y) #:dissolve? #t))))
    no-conflict)))

(test-equal "nested membranes dissolving together empty into the survivor"
  '(1 ((p . 1) (u . 1) (v . 1)))
  (psystem-membranes
   (psystem-step
    (make-psystem '(1 (2 (3))) '((1 p) (2 x) (3 y))
                  `((2 ,(make-rule '(x) '(u) #:dissolve? #t))
                    (3 ,(make-rule '(y) '(v) #:dissolve? #t))))
    no-conflict)))

(test-equal "objects sent into a membrane as it dissolves fall back out"
  '(1 ((a . 1) (y . 1)))
  (psystem-membranes
   (psystem-step
    (make-psystem '(1 (2)) '((1 a) (2 x))
                  `((1 ,(make-rule '(a) '((a in 2))))
                    (2 ,(make-rule '(x) '(y) #:dissolve? #t))))
    no-conflict)))

(let ((after (psystem-step
              (make-psystem '(1 (2)) '((1 a) (2 x))
                            `((1 ,(make-rule '(a) '((a in 2))))
                              (2 ,(make-rule '(x) '() #:dissolve? #t))))
              no-conflict)))
  (test-assert "a rule aimed at a dissolved membrane no longer applies"
    (psystem-halted? after))
  (test-equal "contents of a dissolved membrane" #f (psystem-contents after 2))
  (test-equal "count in a dissolved membrane" 0 (psystem-count after 2 'a))
  (test-assert "stepping a halted system returns it unchanged"
    (eq? after (psystem-step after no-conflict))))

;;; Conservation: rules whose right side is as large as their left side
;;; neither create nor destroy objects, wherever the products go.

;; a and b circulate for ever through all four membranes; the c's leave for
;; the environment in pairs during the first step.
(define shuffling-contents
  '((1 a a a b c c c c c) (2 a b b) (3 b b) (4 a b)))

(define shuffling-rules
  `((1 ,(make-rule '(a) '((a in 2)))
       ,(make-rule '(a) '((b in 3)))
       ,(make-rule '(b) '(a))
       ,(make-rule '(c c) '((e out) (e out))))
    (2 ,(make-rule '(a) '((a in 4)))
       ,(make-rule '(a) '((b out)))
       ,(make-rule '(b) '((a out))))
    (3 ,(make-rule '(b) '((a out))))
    (4 ,(make-rule '(a) '((b out)))
       ,(make-rule '(b) '((a out))))))

(define (shuffling-system)
  (make-psystem '(1 (2 (4)) (3)) shuffling-contents shuffling-rules))

(define shuffling-total
  (apply + (map (lambda (entry) (length (cdr entry))) shuffling-contents)))

(define (conserving? rule)
  (= (multiset-size (rule-lhs rule))
     (apply + (map (lambda (product) (multiset-size (cdr product)))
                   (rule-products rule)))))

(test-equal "rule accessors: left side" '((b . 1) (c . 1))
  (rule-lhs (make-rule '(b c) '((c out) a))))
(test-equal "rule accessors: products by target"
  '((out . ((c . 1))) (here . ((a . 1))))
  (rule-products (make-rule '(b c) '((c out) a))))
(test-assert "rule accessors: dissolve flag"
  (and (rule-dissolve? (make-rule '(a) '() #:dissolve? #t))
       (not (rule-dissolve? (make-rule '(a) '())))))
(test-assert "conserving? tells the two kinds of rule apart"
  (and (conserving? (make-rule '(a b) '((b in 4) (a out))))
       (not (conserving? (make-rule '(a) '(a a))))))

(test-assert "conservation: every rule of the system is conserving"
  (every conserving? (append-map cdr shuffling-rules)))
(test-equal "conservation: the system starts with 16 objects" 16 shuffling-total)
(test-equal "conservation: object total of the initial system"
  shuffling-total
  (psystem-object-total (shuffling-system)))
(test-assert "conservation: the object total never changes over 30 steps"
  (every (lambda (seed)
           (let ((choose (make-seeded-scheduler seed)))
             (let loop ((system (shuffling-system)) (n 0))
               (and (= shuffling-total (psystem-object-total system))
                    (or (= n 30)
                        (loop (psystem-step system choose) (+ n 1)))))))
         seeds))
(test-assert "conservation: the system keeps running and four objects have left"
  (every (lambda (seed)
           (let ((end (psystem-run (shuffling-system) (make-seeded-scheduler seed) 30)))
             (and (= 30 (psystem-steps end))
                  (not (psystem-halted? end))
                  (equal? '((e . 4)) (psystem-environment end)))))
         seeds))
(test-assert "conservation: the objects do move between membranes"
  (let ((trace (psystem-trace (shuffling-system) (make-seeded-scheduler 1) 30)))
    (> (length (delete-duplicates (map car trace))) 20)))
(test-equal "a creating rule changes the total"
  '(1 2 4 8)
  (let loop ((system (single-region '(a) (make-rule '(a) '(a a)))) (n 0) (acc '()))
    (if (= n 4)
        (reverse acc)
        (loop (psystem-step system no-conflict) (+ n 1)
              (cons (psystem-object-total system) acc)))))

;;; Membrane structures are rooted trees: a(n) of them with n membranes.

(test-equal "structure labelled in preorder"
  '(1 (2 (3) (4)) (5))
  (tree->membrane-structure '((() ()) ())))

(do ((n 1 (+ n 1))) ((> n 7))
  (let ((systems (map (lambda (t)
                        (make-psystem (tree->membrane-structure t) '() '()))
                      (trees-of-size n))))
    (test-equal (format #f "~a membranes: the system has the shape it was built from" n)
      (trees-of-size n)
      (map psystem-structure systems))
    (test-equal (format #f "~a membranes: a(~a) distinct structures" n n)
      (a000081 n)
      (length (delete-duplicates (map psystem-structure systems))))
    (test-assert (format #f "~a membranes: membrane count" n)
      (every (lambda (system) (= n (psystem-membrane-count system))) systems))))

;; Dissolving a membrane contracts the edge above it.
(define (contract structure label)
  ;; STRUCTURE without the membrane LABEL, its children moved up.
  (cons (car structure)
        (append-map (lambda (child)
                      (if (equal? label (car child))
                          (map (lambda (grandchild) (contract grandchild label))
                               (cdr child))
                          (list (contract child label))))
                    (cdr structure))))

(define (shape structure)
  (tree-canonical (let strip ((s structure)) (map strip (cdr s)))))

(do ((n 2 (+ n 1))) ((> n 6))
  (test-assert (format #f "~a membranes: dissolving any inner membrane contracts its edge" n)
    (every
     (lambda (t)
       (let ((structure (tree->membrane-structure t)))
         (every
          (lambda (label)
            (let ((after (psystem-step
                          (make-psystem structure `((,label x))
                                        `((,label ,(make-rule '(x) '() #:dissolve? #t))))
                          no-conflict)))
              (and (= (- n 1) (psystem-membrane-count after))
                   (equal? (contract structure label) (psystem-labels after))
                   (equal? (shape (contract structure label))
                           (psystem-structure after)))))
          (iota (- n 1) 2))))
     (trees-of-size n))))

;;; Construction errors.

(test-error "duplicate labels" #t (make-psystem '(1 (2) (2)) '() '()))
(test-error "contents for an unknown membrane" #t (make-psystem '(1) '((2 a)) '()))
(test-error "rules for an unknown membrane" #t
  (make-psystem '(1) '() `((2 ,(make-rule '(a) '(b))))))
(test-error "the skin cannot dissolve" #t
  (make-psystem '(1) '() `((1 ,(make-rule '(a) '() #:dissolve? #t)))))
(test-error "sending into a membrane that does not exist" #t
  (make-psystem '(1) '() `((1 ,(make-rule '(a) '((a in 9)))))))
(test-error "yielding to a rule of another region" #t
  (let ((other (make-rule '(a) '(b))))
    (make-psystem '(1 (2)) '()
                  `((1 ,other)
                    (2 ,(make-rule '(a) '(c) #:yields-to (list other)))))))
(test-error "empty left side" #t (make-rule '() '(a)))
(test-error "bad product" #t (make-rule '(a) '((a up))))
(test-error "yields-to must hold rules" #t (make-rule '(a) '(b) #:yields-to '(1)))
(test-error "not a rule" #t (make-psystem '(1) '() '((1 not-a-rule))))

(test-finish "psystem")
