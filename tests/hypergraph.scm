(use-modules (srfi srfi-1) (srfi srfi-64)
             (tests support) (ghost a000081) (ghost tree) (ghost matula)
             (ghost hypergraph))

(test-begin "hypergraph")

(define (sexps atoms) (map atom->sexp atoms))

;;; Insertion and uniqueness.

(define space (make-atomspace))
(define cat (add-node! space 'Concept "cat"))
(define animal (add-node! space 'Concept "animal"))
(define cat-is-animal (add-link! space 'Inheritance (list cat animal)))

(test-equal "three atoms" 3 (atomspace-size space))
(test-assert "adding a node again returns the same atom"
  (eq? cat (add-node! space 'Concept "cat")))
(test-assert "adding a link again returns the same atom"
  (eq? cat-is-animal (add-link! space 'Inheritance (list cat animal))))
(test-equal "and adds nothing" 3 (atomspace-size space))
(test-assert "same name, different type: a different node"
  (not (eq? cat (add-node! space 'Predicate "cat"))))
(test-assert "ordered links: the other order is a different link"
  (not (eq? cat-is-animal (add-link! space 'Inheritance (list animal cat)))))
(test-assert "node and link predicates"
  (and (node? cat) (not (link? cat)) (link? cat-is-animal) (not (node? cat-is-animal))))
(test-equal "accessors"
  '(Inheritance #f 2 Concept "cat" 0)
  (list (atom-type cat-is-animal) (atom-name cat-is-animal) (atom-arity cat-is-animal)
        (atom-type cat) (atom-name cat) (atom-arity cat)))
(test-equal "find-node" cat (find-node space 'Concept "cat"))
(test-equal "find-node: absent" #f (find-node space 'Concept "unicorn"))
(test-equal "find-link" cat-is-animal (find-link space 'Inheritance (list cat animal)))
(test-equal "find-link: absent" #f (find-link space 'Similarity (list cat animal)))

;; Unordered types hold multisets.
(let ((ab (add-link! space 'Set (list cat animal))))
  (test-assert "unordered links: either order is the same link"
    (eq? ab (add-link! space 'Set (list animal cat))))
  (test-assert "unordered links: multiplicity counts"
    (not (eq? (add-link! space 'Set (list cat))) (add-link! space 'Set (list cat cat)))))
  (test-assert "find-link on an unordered type ignores order"
    (eq? ab (find-link space 'Set (list animal cat)))))

(test-equal "incoming set of cat"
  '((Inheritance (Concept "cat") (Concept "animal"))
    (Inheritance (Concept "animal") (Concept "cat"))
    (Set (Concept "cat") (Concept "animal"))
    (Set (Concept "cat"))
    (Set (Concept "cat") (Concept "cat")))
  (sexps (atom-incoming cat)))
(test-equal "atoms of a type, oldest first"
  '((Concept "cat") (Concept "animal"))
  (sexps (atoms-of-type space 'Concept)))
(test-equal "every atom is listed once"
  (atomspace-size space)
  (length (delete-duplicates (atomspace-atoms space) eq?)))

(test-error "node name must be a string" #t (add-node! space 'Concept 'cat))
(test-error "type must be a symbol" #t (add-node! space "Concept" "cat"))
(test-error "outgoing must hold atoms" #t (add-link! space 'List '(1 2)))
(test-error "atoms of another store are refused" #t
  (add-link! space 'List (list (add-node! (make-atomspace) 'Concept "cat"))))

;;; Truth and attention values.

(test-equal "default truth value" '(1 0)
  (list (tv-strength (atom-tv cat)) (tv-confidence (atom-tv cat))))
(test-equal "default attention value" '(0 0)
  (list (av-sti (atom-av cat)) (av-lti (atom-av cat))))
(set-atom-tv! cat-is-animal (make-truth-value 0.9 0.5))
(set-atom-av! cat (make-attention-value 12 -3))
(test-equal "truth value is stored" '(0.9 0.5)
  (list (tv-strength (atom-tv cat-is-animal)) (tv-confidence (atom-tv cat-is-animal))))
(test-equal "attention value is stored" '(12 -3)
  (list (av-sti (atom-av cat)) (av-lti (atom-av cat))))
(test-equal "re-adding an atom keeps its values" 0.9
  (tv-strength (atom-tv (add-link! space 'Inheritance (list cat animal)))))
(test-error "strength outside [0, 1]" #t (make-truth-value 1.5 0))
(test-error "set-atom-tv! wants a truth value" #t (set-atom-tv! cat 0.5))
(test-error "set-atom-av! wants an attention value" #t (set-atom-av! cat 0.5))

;;; S-expressions.

(define zoo (make-atomspace))
(for-each (lambda (form) (sexp->atom! zoo form))
          '((Inheritance (Concept "cat") (Concept "mammal"))
            (Inheritance (Concept "dog") (Concept "mammal"))
            (Inheritance (Concept "mammal") (Concept "animal"))
            (Inheritance (Concept "trout") (Concept "fish"))
            (Inheritance (Concept "fish") (Concept "animal"))
            (Similarity (Concept "cat") (Concept "cat"))
            (Evaluation (Predicate "chases") (List (Concept "dog") (Concept "cat")))
            (Evaluation (Predicate "chases") (List (Concept "cat") (Concept "trout")))
            (Set (Concept "cat") (Concept "dog") (Concept "trout"))
            (Set (Concept "dog") (Concept "mammal"))))

(test-equal "nodes and links after loading" '(7 2 14)
  (list (length (atoms-of-type zoo 'Concept))
        (length (atoms-of-type zoo 'Predicate))
        (atomspace-size zoo)))
(test-assert "atom -> s-expression -> atom is the identity"
  (every (lambda (atom) (eq? atom (sexp->atom! zoo (atom->sexp atom))))
         (atomspace-atoms zoo)))
(test-assert "find-atom finds what is there"
  (every (lambda (atom) (eq? atom (find-atom zoo (atom->sexp atom))))
         (atomspace-atoms zoo)))
(test-equal "find-atom adds nothing" '(#f 14)
  (list (find-atom zoo '(Inheritance (Concept "cat") (Concept "unicorn")))
        (atomspace-size zoo)))
(test-error "not an atom form" #t (sexp->atom! zoo '("Concept" "cat")))

;;; Pattern matching.

(define (answers pattern name)
  (map (lambda (binding) (atom->sexp (binding-ref binding name)))
       (match-pattern zoo pattern)))

(test-equal "one variable"
  '((Concept "cat") (Concept "dog"))
  (answers '(Inheritance (Variable "$x") (Concept "mammal")) "$x"))
(test-equal "a ground pattern matches once, binding nothing"
  '(())
  (match-pattern zoo '(Inheritance (Concept "cat") (Concept "mammal"))))
(test-equal "a ground pattern that is absent" '()
  (match-pattern zoo '(Inheritance (Concept "cat") (Concept "fish"))))
(test-equal "two variables: every Inheritance link" 5
  (length (match-pattern zoo '(Inheritance (Variable "$x") (Variable "$y")))))
(test-equal "a repeated variable must agree with itself"
  '((Concept "cat"))
  (answers '(Similarity (Variable "$x") (Variable "$x")) "$x"))
(test-equal "which no Inheritance link does" '()
  (match-pattern zoo '(Inheritance (Variable "$x") (Variable "$x"))))
(test-equal "nested links"
  '((Concept "cat") (Concept "trout"))
  (answers '(Evaluation (Predicate "chases") (List (Variable "$a") (Variable "$b"))) "$b"))
(test-equal "a variable may stand for a link"
  '((List (Concept "dog") (Concept "cat")) (List (Concept "cat") (Concept "trout")))
  (answers '(Evaluation (Predicate "chases") (Variable "$args")) "$args"))
(test-equal "typed variable"
  '((Predicate "chases"))
  (delete-duplicates
   (answers '(Evaluation (Variable "$p" Predicate) (Variable "$args" List)) "$p")))
(test-equal "typed variable of the wrong type" '()
  (match-pattern zoo '(Evaluation (Variable "$p" Concept) (Variable "$args"))))
(test-equal "ordered links do not match out of order" '()
  (match-pattern zoo '(Inheritance (Concept "mammal") (Concept "cat"))))
(test-equal "arity must agree" '()
  (match-pattern zoo '(Inheritance (Variable "$x"))))

;; Conjunctions: shared variables join the clauses.
(test-equal "grandparents: x is a y, y is an animal"
  '(((Concept "cat") (Concept "mammal")) ((Concept "dog") (Concept "mammal"))
    ((Concept "trout") (Concept "fish")))
  (map (lambda (binding)
         (list (atom->sexp (binding-ref binding "$x"))
               (atom->sexp (binding-ref binding "$y"))))
       (match-all zoo '((Inheritance (Variable "$x") (Variable "$y"))
                        (Inheritance (Variable "$y") (Concept "animal"))))))
(test-equal "a mammal that chases a mammal"
  '(((Concept "dog") (Concept "cat")))
  (map (lambda (binding)
         (list (atom->sexp (binding-ref binding "$a"))
               (atom->sexp (binding-ref binding "$b"))))
       (match-all zoo '((Evaluation (Predicate "chases") (List (Variable "$a") (Variable "$b")))
                        (Inheritance (Variable "$a") (Concept "mammal"))
                        (Inheritance (Variable "$b") (Concept "mammal"))))))
(test-equal "an unsatisfiable conjunction" '()
  (match-all zoo '((Inheritance (Variable "$x") (Concept "mammal"))
                   (Inheritance (Variable "$x") (Concept "fish")))))

;; Unordered links match in any order.
(test-equal "unordered: the pattern may list the elements in any order"
  '((Concept "mammal"))
  (answers '(Set (Variable "$x") (Concept "dog")) "$x"))
(test-equal "unordered: two variables take the elements both ways round" 2
  (length (match-pattern zoo '(Set (Variable "$x") (Variable "$y")))))

;; Globs take what is left.
(test-equal "glob: the sets that contain dog, and their other members"
  '(((Concept "cat") (Concept "trout")) ((Concept "mammal")))
  (map (lambda (binding) (sexps (binding-ref binding "$rest")))
       (match-pattern zoo '(Set (Concept "dog") (Glob "$rest")))))
(test-equal "glob: on an ordered link it takes the tail"
  '(((Concept "cat")) ((Concept "trout")))
  (map (lambda (binding) (sexps (binding-ref binding "$rest")))
       (match-pattern zoo '(List (Variable "$first") (Glob "$rest")))))
(test-equal "glob: may take nothing" 1
  (length (match-pattern zoo '(Similarity (Concept "cat") (Concept "cat") (Glob "$none")))))
(test-error "a glob must come last" #t
  (match-pattern zoo '(Set (Glob "$rest") (Concept "dog"))))
(test-error "not a pattern" #t (match-pattern zoo 'x))

;; Rewriting with a binding.
(let* ((before (atomspace-size zoo))
       (made (map (lambda (binding)
                    (instantiate! zoo '(Inheritance (Variable "$x") (Concept "animal"))
                                  binding))
                  (match-all zoo '((Inheritance (Variable "$x") (Variable "$y"))
                                   (Inheritance (Variable "$y") (Concept "animal")))))))
  (test-equal "instantiate!: transitive closure, one step"
    '((Inheritance (Concept "cat") (Concept "animal"))
      (Inheritance (Concept "dog") (Concept "animal"))
      (Inheritance (Concept "trout") (Concept "animal")))
    (sexps made))
  (test-equal "instantiate!: three links were added" (+ before 3) (atomspace-size zoo)))
(test-equal "instantiate!: a glob splices its atoms"
  '(List (Concept "x") (Concept "cat") (Concept "trout"))
  (atom->sexp
   (instantiate! zoo '(List (Concept "x") (Glob "$rest"))
                 (car (match-pattern zoo '(Set (Concept "dog") (Concept "cat") (Glob "$rest")))))))
(test-error "instantiate!: unbound variable" #t
  (instantiate! zoo '(List (Variable "$nope")) '()))

;;; Removal.

(let* ((s (make-atomspace))
       (a (add-node! s 'Concept "a"))
       (b (add-node! s 'Concept "b"))
       (ab (add-link! s 'List (list a b)))
       (outer (add-link! s 'List (list ab a))))
  (test-error "an atom inside a link cannot be removed alone" #t (remove-atom! s a))
  (test-equal "nothing was removed" 4 (atomspace-size s))
  (test-assert "a link nothing contains can be removed" (remove-atom! s outer))
  (test-equal "it is gone" '(3 #f) (list (atomspace-size s) (find-link s 'List (list ab a))))
  (test-equal "and has left the incoming sets" '((List (Concept "a") (Concept "b")))
    (sexps (atom-incoming a)))
  (test-assert "recursive removal takes the containing links too"
    (remove-atom! s a #:recursive? #t))
  (test-equal "only b is left" '((Concept "b")) (sexps (atomspace-atoms s)))
  (test-equal "b has no incoming links" '() (atom-incoming b))
  (test-equal "removing an atom that is not there" #f (remove-atom! s a))
  (test-assert "a removed atom can be added afresh"
    (not (eq? a (add-node! s 'Concept "a")))))

;;; Trees as nested links keyed by Matula number.

(define forest (make-atomspace))

(test-equal "the tree [[.],.] as nested links"
  '(Tree (Tree) (Tree (Tree)))
  (atom->sexp (add-tree! forest '((()) ()))))
(test-assert "the order of the subtrees does not matter"
  (eq? (add-tree! forest '((()) ())) (add-tree! forest '(() (())))))
(test-equal "its subtrees are stored too, once each: 3 trees, 3 keys, 3 numbers"
  '(3 3 3)
  (list (length (atoms-of-type forest 'Tree))
        (length (atoms-of-type forest 'Matula))
        (length (atoms-of-type forest 'Number))))

(do ((n 1 (+ n 1))) ((> n 8))
  (let* ((trees (trees-of-size n))
         (atoms (map (lambda (t) (add-tree! forest t)) trees)))
    (test-equal (format #f "size ~a: tree -> atom -> tree" n)
      trees
      (map atom->tree atoms))
    (test-equal (format #f "size ~a: each atom is keyed by its Matula number" n)
      (map tree->matula trees)
      (map atom->matula atoms))
    (test-assert (format #f "size ~a: lookup by Matula number returns the atom" n)
      (every (lambda (t atom) (eq? atom (tree-atom forest (tree->matula t))))
             trees atoms))
    ;; Every subtree of a tree with at most n nodes is such a tree, so the
    ;; store now holds exactly the trees with at most n nodes.
    (test-equal (format #f "size ~a: the store holds a(1) + ... + a(~a) trees" n n)
      (apply + (map a000081 (iota n 1)))
      (length (atoms-of-type forest 'Tree)))))

(test-equal "no tree is keyed by a number that was never added" #f
  (tree-atom forest 1000003))
(test-assert "tree-atom? tells tree atoms from others"
  (and (tree-atom? (tree-atom forest 6))
       (not (tree-atom? (find-node forest 'Number "6")))))

;; The matcher sees trees as multisets of subtrees.
(test-equal "trees with a leaf and a two-node chain below the root, by pattern"
  (sort (map tree->matula
             (filter (lambda (t) (and (member '() t) (member '(()) t)))
                     (append-map trees-of-size (iota 8 1))))
        <)
  (sort (delete-duplicates
         (map (lambda (binding) (atom->matula (binding-ref binding "$t")))
              (match-all forest '((Matula (Variable "$n") (Variable "$t"))
                                  (Tree (Tree) (Tree (Tree)) (Glob "$others"))))))
        <))

(test-error "a tree whose Matula number is over the limit is refused" #t
  (add-tree! forest (chain-tree 40)))
(test-error "add-tree! wants a tree" #t (add-tree! forest '(1 2)))
(test-error "a store without unordered Tree links cannot hold trees" #t
  (add-tree! (make-atomspace #:unordered '(Set)) '()))

;;; Trees as hypergraphs on their nodes.

(define positions (make-atomspace))

(test-equal "hyperedges of the tree 0 <- {1 <- {2, 3}, 4}"
  '((Attention (Position "0") (Set (Position "0")))
    (Attention (Position "1") (Set (Position "0") (Position "1")))
    (Attention (Position "2") (Set (Position "0") (Position "1") (Position "2")))
    (Attention (Position "3") (Set (Position "0") (Position "1") (Position "3")))
    (Attention (Position "4") (Set (Position "0") (Position "4"))))
  (sexps (add-ancestor-hyperedges! positions #(0 0 1 1 0))))
(test-error "not a rooted parent map" #t
  (add-ancestor-hyperedges! positions #(1 0)))

(do ((n 1 (+ n 1))) ((> n 6))
  (let ((maps (increasing-parent-maps n)))
    (test-assert (format #f "~a points: the hyperedges determine the parent map" n)
      (every (lambda (f)
               (let ((s (make-atomspace)))
                 (equal? f (ancestor-hyperedges->parent-map
                            (reverse (add-ancestor-hyperedges! s f))))))
             maps))
    (test-assert (format #f "~a points: hyperedge i joins i and its ancestors" n)
      (every (lambda (f)
               (let* ((s (make-atomspace))
                      (edges (add-ancestor-hyperedges! s f)))
                 (every (lambda (edge i)
                          (let depth ((j i) (d 1))
                            (if (= j (vector-ref f j))
                                (= d (atom-arity (cadr (atom-outgoing edge))))
                                (depth (vector-ref f j) (+ d 1)))))
                        edges (iota n))))
             maps))))

;; The chain gives the causal sets {0..i}; the root lies in every set.
(let* ((s (make-atomspace))
       (edges (add-ancestor-hyperedges! s (tree->parent-map (chain-tree 6)))))
  (test-equal "chain: point i is joined to 0 .. i"
    (map (lambda (i) (map number->string (iota (+ i 1)))) (iota 6))
    (map (lambda (edge) (map atom-name (atom-outgoing (cadr (atom-outgoing edge)))))
         edges))
  (test-equal "chain: the root is in all six sets" 6
    (length (match-pattern s '(Set (Position "0") (Glob "$others"))))))

;; A map of trees that keeps the root and the parents carries hyperedges to
;; hyperedges; a map that does not, does not.
(let* ((s (make-atomspace))
       (small (add-ancestor-hyperedges! s #(0 0 1)))        ; chain 0 <- 1 <- 2
       (large (add-ancestor-hyperedges! s #(0 0 0 1 3)))    ; 0 <- {1 <- 3 <- 4, 2}
       (along (lambda (images)
                (lambda (node)
                  (add-node! s 'Position
                             (number->string
                              (vector-ref images (string->number (atom-name node)))))))))
  (test-assert "an embedding sends every hyperedge to a hyperedge"
    (every (lambda (edge) (memq (map-atom! s (along #(0 1 3)) edge) large)) small))
  (test-assert "a map that breaks the parent relation does not"
    (not (every (lambda (edge) (memq (map-atom! s (along #(0 2 3)) edge) large)) small)))
  (test-assert "map-atom! respects composition"
    (every (lambda (edge)
             (eq? (map-atom! s (along #(0 3 4)) edge)
                  (map-atom! s (along #(0 3 4 1 2))
                             (map-atom! s (along #(0 1 2)) edge))))
           small)))

(test-finish "hypergraph")
