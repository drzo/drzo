;;; (ghost psystem) - transition P systems (membrane systems, Paun 2000)
;;;
;;; A membrane structure is a rooted tree: the root is the skin membrane and
;;; every membrane encloses a region holding a multiset of objects.  There
;;; are therefore a(n) membrane structures with n membranes.
;;;
;;; Each region has evolution rules u -> v.  The left side u is a multiset
;;; of objects; the right side sends each product to a target: the same
;;; region (here), the enclosing region (out; from the skin, the
;;; environment) or a membrane directly inside (in).  A rule may also
;;; dissolve its membrane, whose objects and inner membranes then pass to
;;; the enclosing region.
;;;
;;; A step is maximally parallel: every region picks a multiset of rule
;;; instances from its contents at the start of the step until no further
;;; rule fits what is left, and all products arrive at the end of the step,
;;; before any dissolution.  Where rules compete for the same objects, a
;;; scheduler chooses.
;;;
;;; Priority is a strict partial order on the rules of a region, given by
;;; listing for each rule the rules it yields to.  It is strong: a rule is
;;; barred for the whole step if any rule above it is applicable at the
;;; start of the step.

(define-module (ghost psystem)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ghost tree)
  #:export (list->multiset
            multiset-count
            multiset-size

            make-rule
            rule?
            rule-lhs
            rule-products
            rule-dissolve?
            rule-yields-to

            make-psystem
            psystem?
            psystem-membranes
            psystem-labels
            psystem-structure
            psystem-membrane-count
            psystem-contents
            psystem-count
            psystem-environment
            psystem-object-total
            psystem-steps
            psystem-halted?
            psystem-step
            psystem-run
            psystem-trace

            first-rule-scheduler
            make-seeded-scheduler
            tree->membrane-structure))

;;; Multisets of objects: association lists (object . count) with positive
;;; counts, sorted by object name, so equal multisets are `equal?'.

(define (object<? a b)
  (string<? (symbol->string a) (symbol->string b)))

(define (list->multiset objects)
  "The multiset of the symbols in the list OBJECTS."
  (unless (and (list? objects) (every symbol? objects))
    (error "list->multiset: expected a list of symbols:" objects))
  (let loop ((sorted (sort objects object<?)) (acc '()))
    (cond ((null? sorted) (reverse acc))
          ((and (pair? acc) (eq? (caar acc) (car sorted)))
           (loop (cdr sorted)
                 (cons (cons (caar acc) (+ 1 (cdar acc))) (cdr acc))))
          (else (loop (cdr sorted) (cons (cons (car sorted) 1) acc))))))

(define (multiset-count ms object)
  "The number of copies of OBJECT in the multiset MS."
  (or (assq-ref ms object) 0))

(define (multiset-size ms)
  "The number of objects in MS, counted with multiplicity."
  (fold (lambda (entry sum) (+ sum (cdr entry))) 0 ms))

(define (multiset-add a b)
  (cond ((null? a) b)
        ((null? b) a)
        ((eq? (caar a) (caar b))
         (cons (cons (caar a) (+ (cdar a) (cdar b)))
               (multiset-add (cdr a) (cdr b))))
        ((object<? (caar a) (caar b)) (cons (car a) (multiset-add (cdr a) b)))
        (else (cons (car b) (multiset-add a (cdr b))))))

(define (multiset-scale ms k)
  (map (lambda (entry) (cons (car entry) (* k (cdr entry)))) ms))

(define (multiset-quotient a b)
  ;; The largest k with k copies of the non-empty B inside A.
  (apply min (map (lambda (entry)
                    (quotient (multiset-count a (car entry)) (cdr entry)))
                  b)))

(define (multiset-subtract a b)
  ;; A minus B, for B inside A.
  (filter-map (lambda (entry)
                (let ((left (- (cdr entry) (multiset-count b (car entry)))))
                  (and (> left 0) (cons (car entry) left))))
              a))

;;; Rules.

(define-record-type <rule>
  (%make-rule lhs products dissolve? yields-to)
  rule?
  (lhs rule-lhs)
  (products rule-products)
  (dissolve? rule-dissolve?)
  (yields-to rule-yields-to))

(define (parse-product product)
  ;; -> (object . target), target being here, out or (in . label).
  (cond ((symbol? product) (cons product 'here))
        ((and (list? product) (= 2 (length product))
              (symbol? (car product)) (memq (cadr product) '(here out)))
         (cons (car product) (cadr product)))
        ((and (list? product) (= 3 (length product))
              (symbol? (car product)) (eq? 'in (cadr product)))
         (cons (car product) (cons 'in (caddr product))))
        (else (error "make-rule: bad product:" product))))

(define* (make-rule lhs products #:key (dissolve? #f) (yields-to '()))
  "The rule consuming the objects in the list LHS and producing PRODUCTS.
Each product is OBJECT (it stays here), (OBJECT here), (OBJECT out) or
(OBJECT in LABEL).  With DISSOLVE? the membrane dissolves after the step.
YIELDS-TO lists the rules of the same region that have priority over this
one."
  (when (or (not (list? lhs)) (null? lhs))
    (error "make-rule: the left side must be a non-empty list of objects:" lhs))
  (unless (list? products)
    (error "make-rule: products must be a list:" products))
  (unless (and (list? yields-to) (every rule? yields-to))
    (error "make-rule: yields-to must be a list of rules:" yields-to))
  (let* ((parsed (map parse-product products))
         (targets (delete-duplicates (map cdr parsed))))
    (%make-rule (list->multiset lhs)
                ;; One (target . multiset) entry per distinct target.
                (map (lambda (target)
                       (cons target
                             (list->multiset
                              (filter-map (lambda (p)
                                            (and (equal? target (cdr p)) (car p)))
                                          parsed))))
                     targets)
                (and dissolve? #t)
                yields-to)))

;;; Systems.  A membrane is the list (label contents child ...).

(define-record-type <psystem>
  (%make-psystem skin rules environment steps)
  psystem?
  (skin psystem-skin)
  (rules psystem-rules)
  (environment psystem-environment)
  (steps psystem-steps))

(define (node-label node) (car node))
(define (node-contents node) (cadr node))
(define (node-children node) (cddr node))

(define (structure-labels structure)
  (unless (pair? structure)
    (error "make-psystem: a membrane must be a list (label child ...):" structure))
  (unless (list? (cdr structure))
    (error "make-psystem: the inner membranes must form a list:" structure))
  (cons (car structure) (append-map structure-labels (cdr structure))))

(define (make-psystem structure contents rules)
  "A P system.  STRUCTURE is the nested list (LABEL CHILD ...), outermost
membrane first.  CONTENTS maps labels to lists of objects and RULES maps
labels to lists of rules; both are association lists and may leave labels
out."
  (let ((labels (structure-labels structure)))
    (unless (= (length labels) (length (delete-duplicates labels)))
      (error "make-psystem: membrane labels must be distinct:" labels))
    (for-each
     (lambda (entry)
       (unless (and (pair? entry) (member (car entry) labels))
         (error "make-psystem: contents for an unknown membrane:" entry)))
     contents)
    (for-each
     (lambda (entry)
       (unless (and (pair? entry) (member (car entry) labels))
         (error "make-psystem: rules for an unknown membrane:" entry))
       (for-each
        (lambda (rule)
          (unless (rule? rule)
            (error "make-psystem: not a rule:" rule))
          (when (and (rule-dissolve? rule) (equal? (car entry) (car structure)))
            (error "make-psystem: the skin membrane cannot dissolve"))
          (unless (every (lambda (above) (memq above (cdr entry)))
                         (rule-yields-to rule))
            (error "make-psystem: a rule yields to a rule of another region:"
                   (car entry)))
          (for-each
           (lambda (product)
             (when (and (pair? (car product))
                        (not (member (cdar product) labels)))
               (error "make-psystem: rule sends into an unknown membrane:"
                      (cdar product))))
           (rule-products rule)))
        (cdr entry)))
     rules)
    (%make-psystem
     (let build ((structure structure))
       (cons* (car structure)
              (list->multiset (or (assoc-ref contents (car structure)) '()))
              (map build (cdr structure))))
     rules
     '()
     0)))

(define (psystem-membranes system)
  "The current configuration as the nested list (LABEL CONTENTS CHILD ...)."
  (psystem-skin system))

(define (psystem-labels system)
  "The current membrane structure as the nested list (LABEL CHILD ...)."
  (let strip ((node (psystem-skin system)))
    (cons (node-label node) (map strip (node-children node)))))

(define (psystem-structure system)
  "The shape of the current membrane structure, as a canonical tree."
  (tree-canonical
   (let shape ((node (psystem-skin system)))
     (map shape (node-children node)))))

(define (all-nodes node)
  (cons node (append-map all-nodes (node-children node))))

(define (psystem-membrane-count system)
  (length (all-nodes (psystem-skin system))))

(define (psystem-contents system label)
  "The multiset in the membrane LABEL, or #f if there is no such membrane."
  (let ((node (find (lambda (node) (equal? label (node-label node)))
                    (all-nodes (psystem-skin system)))))
    (and node (node-contents node))))

(define (psystem-count system label object)
  "The number of copies of OBJECT in the membrane LABEL; 0 if it is gone."
  (multiset-count (or (psystem-contents system label) '()) object))

(define (psystem-object-total system)
  "The number of objects in all membranes and the environment."
  (fold (lambda (node sum) (+ sum (multiset-size (node-contents node))))
        (multiset-size (psystem-environment system))
        (all-nodes (psystem-skin system))))

;;; Evolution.

(define (region-rules system label)
  (or (assoc-ref (psystem-rules system) label) '()))

(define (fits? rule contents)
  (every (lambda (entry) (>= (multiset-count contents (car entry)) (cdr entry)))
         (rule-lhs rule)))

(define (applicable? rule contents child-labels)
  (and (fits? rule contents)
       (every (lambda (product)
                (let ((target (car product)))
                  (or (symbol? target) (member (cdr target) child-labels))))
              (rule-products rule))))

(define (rules-above rule)
  ;; Every rule above RULE in the priority order.
  (append-map (lambda (above) (cons above (rules-above above)))
              (rule-yields-to rule)))

(define (usable-rules rules contents child-labels)
  ;; The applicable rules that no applicable rule has priority over.
  (let ((applicable (filter (lambda (rule)
                              (applicable? rule contents child-labels))
                            rules)))
    (remove (lambda (rule)
              (any (lambda (above) (memq above applicable)) (rules-above rule)))
            applicable)))

(define (uncontested rules)
  ;; A rule in RULES none of whose objects another of them consumes, or #f.
  (find (lambda (rule)
          (every (lambda (other)
                   (or (eq? other rule)
                       (not (any (lambda (entry) (assq (car entry) (rule-lhs other)))
                                 (rule-lhs rule)))))
                 rules))
        rules))

(define (choose-applications label rules contents child-labels choose)
  ;; -> (applications . remaining), applications being (rule . count) pairs.
  ;; A rule that shares no object with the other candidates is used as often
  ;; as it fits in every maximal choice, so only real conflicts reach CHOOSE.
  (let loop ((remaining contents)
             (candidates (usable-rules rules contents child-labels))
             (applied '()))
    (let ((candidates (filter (lambda (rule) (fits? rule remaining)) candidates)))
      (if (null? candidates)
          (cons (reverse applied) remaining)
          (let ((free (uncontested candidates)))
            (if free
                (let ((k (multiset-quotient remaining (rule-lhs free))))
                  (loop (multiset-subtract remaining
                                           (multiset-scale (rule-lhs free) k))
                        candidates
                        (cons (cons free k) applied)))
                (let ((i (choose label candidates)))
                  (unless (and (integer? i) (exact? i) (<= 0 i)
                               (< i (length candidates)))
                    (error "psystem-step: scheduler returned a bad index:" i))
                  (let ((rule (list-ref candidates i)))
                    (loop (multiset-subtract remaining (rule-lhs rule))
                          candidates
                          (cons (cons rule 1) applied))))))))))

(define (psystem-halted? system)
  "True if no rule is applicable in any region."
  (every (lambda (node)
           (null? (usable-rules (region-rules system (node-label node))
                                (node-contents node)
                                (map node-label (node-children node)))))
         (all-nodes (psystem-skin system))))

(define (psystem-step system choose)
  "One maximally parallel step.  (CHOOSE label rules) is called when the
rules in the list RULES compete for objects in the region LABEL, and must
return the index of the one to use next.  A halted system is returned
unchanged."
  (if (psystem-halted? system)
      system
      (let ((arrivals (make-hash-table))
            (dissolving (make-hash-table))
            (remaining (make-hash-table))
            (environment (psystem-environment system)))
        (define (deliver! label ms)
          (hash-set! arrivals label
                     (multiset-add (hash-ref arrivals label '()) ms)))
        ;; Every region chooses from the configuration at the start of the
        ;; step; products are only collected here.
        (let visit ((node (psystem-skin system)) (parent #f))
          (let* ((label (node-label node))
                 (choice (choose-applications
                          label
                          (region-rules system label)
                          (node-contents node)
                          (map node-label (node-children node))
                          choose)))
            (hash-set! remaining label (cdr choice))
            (for-each
             (lambda (application)
               (let ((rule (car application)) (count (cdr application)))
                 (when (rule-dissolve? rule)
                   (hash-set! dissolving label #t))
                 (for-each
                  (lambda (product)
                    (let ((target (car product))
                          (ms (multiset-scale (cdr product) count)))
                      (cond ((eq? target 'here) (deliver! label ms))
                            ((pair? target) (deliver! (cdr target) ms))
                            (parent (deliver! (node-label parent) ms))
                            (else
                             (set! environment (multiset-add environment ms))))))
                  (rule-products rule))))
             (car choice))
            (for-each (lambda (child) (visit child node))
                      (node-children node))))
        ;; Deliver, then dissolve: a dissolved membrane hands its objects
        ;; and its inner membranes to the nearest surviving ancestor.
        (let ((rebuilt
               (let rebuild ((node (psystem-skin system)))
                 (let* ((label (node-label node))
                        (inner (map rebuild (node-children node)))
                        (contents (fold multiset-add
                                        (multiset-add
                                         (hash-ref remaining label)
                                         (hash-ref arrivals label '()))
                                        (map cdr inner)))
                        (children (append-map car inner)))
                   (if (hash-ref dissolving label)
                       (cons children contents)
                       (cons (list (cons* label contents children)) '()))))))
          (%make-psystem (car (car rebuilt))
                         (psystem-rules system)
                         environment
                         (+ 1 (psystem-steps system)))))))

(define* (psystem-run system choose #:optional (max-steps 1000))
  "Step SYSTEM until it halts or MAX-STEPS steps have been taken."
  (let loop ((system system) (n 0))
    (if (or (= n max-steps) (psystem-halted? system))
        system
        (loop (psystem-step system choose) (+ n 1)))))

(define* (psystem-trace system choose #:optional (max-steps 1000))
  "The configurations from SYSTEM to where `psystem-run' stops, each as the
pair (membranes . environment)."
  (let loop ((system system) (n 0) (acc '()))
    (let ((acc (cons (cons (psystem-membranes system)
                           (psystem-environment system))
                     acc)))
      (if (or (= n max-steps) (psystem-halted? system))
          (reverse acc)
          (loop (psystem-step system choose) (+ n 1) acc)))))

;;; Schedulers.

(define (first-rule-scheduler)
  "Always take the first competing rule, in the order the rules were given."
  (lambda (label rules) 0))

(define (make-seeded-scheduler seed)
  "A scheduler choosing uniformly at random among the competing rules, from
a private generator seeded with SEED."
  (let ((state (seed->random-state seed)))
    (lambda (label rules) (random (length rules) state))))

(define (tree->membrane-structure t)
  "The membrane structure (LABEL CHILD ...) shaped like the tree T, with
the membranes numbered from 1 in preorder, so the skin is 1."
  (let ((next 0))
    (let visit ((t t))
      (set! next (+ next 1))
      (let ((label next))
        (let loop ((children t) (acc '()))
          (if (null? children)
              (cons label (reverse acc))
              (loop (cdr children) (cons (visit (car children)) acc))))))))
