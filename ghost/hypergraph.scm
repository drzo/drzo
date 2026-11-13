;;; (ghost hypergraph) - a typed hypergraph store with a pattern matcher
;;;
;;; In the style of the OpenCog AtomSpace.  An atom is a node (a type and a
;;; name) or a link (a type and a list of atoms, its outgoing set).  A link
;;; may join any number of atoms and may contain links, so the store is a
;;; hypergraph.  Atoms are unique: adding a node or link that is already
;;; present returns the atom that is there.  Every atom carries a truth
;;; value and an attention value.
;;;
;;; Link types listed as unordered when the store is made hold a multiset:
;;; their outgoing atoms are kept in a canonical order, so (Set a b) and
;;; (Set b a) are one atom, while (Set a a) and (Set a) are two.
;;;
;;; Atoms are written as s-expressions: (Type "name") is a node and
;;; (Type atom ...) is a link.  Patterns are atoms with holes:
;;;
;;;   (Variable "$x")        any atom
;;;   (Variable "$x" Type)   any atom of that type
;;;   (Glob "$rest")         as the last element of a link: all the atoms
;;;                          not matched by the elements before it
;;;
;;; Only storage and matching are here.  Inference over truth values (PLN),
;;; attention allocation (ECAN), evolutionary search (MOSES) and language
;;; parsing (RelEx) are not; the accessors below are what they would use.

(define-module (ghost hypergraph)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ghost tree)
  #:use-module (ghost matula)
  #:export (make-atomspace
            atomspace?
            atomspace-size
            atomspace-atoms
            atomspace-unordered-types
            atoms-of-type

            add-node!
            add-link!
            find-node
            find-link
            remove-atom!

            atom?
            node?
            link?
            atom-id
            atom-type
            atom-name
            atom-outgoing
            atom-arity
            atom-incoming

            make-truth-value
            truth-value?
            tv-strength
            tv-confidence
            make-attention-value
            attention-value?
            av-sti
            av-lti
            atom-tv
            set-atom-tv!
            atom-av
            set-atom-av!

            atom->sexp
            sexp->atom!
            find-atom
            map-atom!

            match-pattern
            match-all
            binding-ref
            instantiate!

            add-tree!
            tree-atom
            tree-atom?
            atom->tree
            atom->matula

            add-ancestor-hyperedges!
            ancestor-hyperedges->parent-map))

;;; Truth and attention values.

(define-record-type <truth-value>
  (%make-truth-value strength confidence)
  truth-value?
  (strength tv-strength)
  (confidence tv-confidence))

(define (unit-interval? x)
  (and (real? x) (<= 0 x 1)))

(define (make-truth-value strength confidence)
  "A truth value: STRENGTH and CONFIDENCE, both in [0, 1]."
  (unless (and (unit-interval? strength) (unit-interval? confidence))
    (error "make-truth-value: strength and confidence must lie in [0, 1]:"
           strength confidence))
  (%make-truth-value strength confidence))

(define-record-type <attention-value>
  (%make-attention-value sti lti)
  attention-value?
  (sti av-sti)
  (lti av-lti))

(define (make-attention-value sti lti)
  "An attention value: short-term and long-term importance."
  (unless (and (real? sti) (real? lti))
    (error "make-attention-value: importances must be real numbers:" sti lti))
  (%make-attention-value sti lti))

;; What an atom has until it is told otherwise: asserted with no evidence,
;; and of no importance.
(define default-tv (%make-truth-value 1 0))
(define default-av (%make-attention-value 0 0))

;;; Atoms and the store.

(define-record-type <atom>
  (%make-atom space id type name outgoing tv av incoming)
  atom?
  (space atom-space set-atom-space!)
  (id atom-id)
  (type atom-type)
  (name atom-name)
  (outgoing atom-outgoing)
  (tv atom-tv %set-atom-tv!)
  (av atom-av %set-atom-av!)
  (incoming %atom-incoming set-atom-incoming!))

(define-record-type <atomspace>
  (%make-atomspace nodes links by-type unordered next-id size)
  atomspace?
  (nodes space-nodes)
  (links space-links)
  (by-type space-by-type)
  (unordered atomspace-unordered-types)
  (next-id space-next-id set-space-next-id!)
  (size atomspace-size set-space-size!))

(define* (make-atomspace #:key (unordered '(Set Tree)))
  "An empty store.  UNORDERED lists the link types that hold multisets."
  (unless (and (list? unordered) (every symbol? unordered))
    (error "make-atomspace: unordered must be a list of type symbols:" unordered))
  (%make-atomspace (make-hash-table) (make-hash-table) (make-hash-table)
                   unordered 1 0))

(define (node? x) (and (atom? x) (string? (atom-name x))))
(define (link? x) (and (atom? x) (not (atom-name x))))
(define (atom-arity atom) (length (atom-outgoing atom)))

(define (by-id atoms)
  (sort atoms (lambda (a b) (< (atom-id a) (atom-id b)))))

(define (atom-incoming atom)
  "The links that contain ATOM, oldest first."
  (by-id (%atom-incoming atom)))

(define (atoms-of-type space type)
  "The atoms of SPACE whose type is TYPE, oldest first."
  (by-id (hashq-ref (space-by-type space) type '())))

(define (atomspace-atoms space)
  "Every atom of SPACE, oldest first."
  (by-id (hash-fold (lambda (type atoms acc) (append atoms acc))
                    '()
                    (space-by-type space))))

(define (unordered-type? space type)
  (and (memq type (atomspace-unordered-types space)) #t))

(define (check-type who type)
  (unless (symbol? type)
    (error (string-append who ": the type must be a symbol:") type)))

(define (register! space atom)
  (let ((table (space-by-type space)))
    (hashq-set! table (atom-type atom)
                (cons atom (hashq-ref table (atom-type atom) '()))))
  (set-space-next-id! space (+ 1 (space-next-id space)))
  (set-space-size! space (+ 1 (atomspace-size space)))
  atom)

(define (find-node space type name)
  "The node of TYPE named NAME in SPACE, or #f."
  (hash-ref (space-nodes space) (cons type name)))

(define (add-node! space type name)
  "The node of TYPE named NAME, added to SPACE if it is not there."
  (check-type "add-node!" type)
  (unless (string? name)
    (error "add-node!: the name must be a string:" name))
  (or (find-node space type name)
      (let ((atom (%make-atom space (space-next-id space) type name '()
                              default-tv default-av '())))
        (hash-set! (space-nodes space) (cons type name) atom)
        (register! space atom))))

(define (canonical-outgoing space type outgoing)
  (if (unordered-type? space type) (by-id outgoing) outgoing))

(define (link-key type outgoing)
  (cons type (map atom-id outgoing)))

(define (check-outgoing who space outgoing)
  (unless (and (list? outgoing)
               (every (lambda (x) (and (atom? x) (eq? space (atom-space x))))
                      outgoing))
    (error (string-append who ": the outgoing set must be a list of atoms of this store:")
           outgoing)))

(define (find-link space type outgoing)
  "The link of TYPE over the atoms OUTGOING in SPACE, or #f."
  (check-outgoing "find-link" space outgoing)
  (hash-ref (space-links space)
            (link-key type (canonical-outgoing space type outgoing))))

(define (add-link! space type outgoing)
  "The link of TYPE over the list of atoms OUTGOING, added to SPACE if it
is not there."
  (check-type "add-link!" type)
  (check-outgoing "add-link!" space outgoing)
  (let* ((outgoing (canonical-outgoing space type outgoing))
         (key (link-key type outgoing)))
    (or (hash-ref (space-links space) key)
        (let ((atom (%make-atom space (space-next-id space) type #f outgoing
                                default-tv default-av '())))
          (hash-set! (space-links space) key atom)
          ;; An atom repeated in the outgoing set still gets one entry.
          (for-each (lambda (target)
                      (set-atom-incoming! target
                                          (cons atom (%atom-incoming target))))
                    (delete-duplicates outgoing eq?))
          (register! space atom)))))

(define* (remove-atom! space atom #:key (recursive? #f))
  "Remove ATOM from SPACE; #f if it is not there.  An atom inside a link
can only go together with that link: with RECURSIVE? the links that contain
it are removed first, otherwise it is an error."
  (cond
   ((not (and (atom? atom) (eq? space (atom-space atom)))) #f)
   (else
    (let ((incoming (%atom-incoming atom)))
      (unless (null? incoming)
        (unless recursive?
          (error "remove-atom!: the atom is inside a link:" (atom->sexp atom)))
        (for-each (lambda (link) (remove-atom! space link #:recursive? #t))
                  incoming)))
    (if (node? atom)
        (hash-remove! (space-nodes space) (cons (atom-type atom) (atom-name atom)))
        (begin
          (hash-remove! (space-links space)
                        (link-key (atom-type atom) (atom-outgoing atom)))
          (for-each (lambda (target)
                      (set-atom-incoming! target
                                          (delq atom (%atom-incoming target))))
                    (delete-duplicates (atom-outgoing atom) eq?))))
    (let ((table (space-by-type space)))
      (hashq-set! table (atom-type atom)
                  (delq atom (hashq-ref table (atom-type atom) '()))))
    (set-space-size! space (- (atomspace-size space) 1))
    (set-atom-space! atom #f)
    #t)))

(define (set-atom-tv! atom tv)
  (unless (truth-value? tv)
    (error "set-atom-tv!: not a truth value:" tv))
  (%set-atom-tv! atom tv))

(define (set-atom-av! atom av)
  (unless (attention-value? av)
    (error "set-atom-av!: not an attention value:" av))
  (%set-atom-av! atom av))

;;; S-expressions.

(define (atom->sexp atom)
  "ATOM as (Type \"name\") or (Type atom ...)."
  (if (node? atom)
      (list (atom-type atom) (atom-name atom))
      (cons (atom-type atom) (map atom->sexp (atom-outgoing atom)))))

(define (node-form? form)
  (and (pair? form) (symbol? (car form))
       (pair? (cdr form)) (string? (cadr form)) (null? (cddr form))))

(define (link-form? form)
  (and (pair? form) (symbol? (car form)) (list? (cdr form))
       (not (node-form? form))))

(define (check-form who form)
  (unless (or (node-form? form) (link-form? form))
    (error (string-append who ": not an atom form:") form)))

(define (sexp->atom! space form)
  "Add the atom written as FORM, and the atoms inside it, to SPACE."
  (check-form "sexp->atom!" form)
  (if (node-form? form)
      (add-node! space (car form) (cadr form))
      (add-link! space (car form)
                 ;; Left to right, so ids follow the order of writing.
                 (let loop ((forms (cdr form)) (acc '()))
                   (if (null? forms)
                       (reverse acc)
                       (loop (cdr forms)
                             (cons (sexp->atom! space (car forms)) acc)))))))

(define (find-atom space form)
  "The atom of SPACE written as FORM, or #f; nothing is added."
  (check-form "find-atom" form)
  (if (node-form? form)
      (find-node space (car form) (cadr form))
      (let ((outgoing (map (lambda (f) (find-atom space f)) (cdr form))))
        (and (every identity outgoing)
             (find-link space (car form) outgoing)))))

(define (map-atom! space f atom)
  "The image of ATOM under the map F on nodes: each node n becomes (F n)
and each link is rebuilt over the images of its outgoing atoms.  The images
are added to SPACE."
  (if (node? atom)
      (f atom)
      (add-link! space (atom-type atom)
                 (map (lambda (target) (map-atom! space f target))
                      (atom-outgoing atom)))))

;;; Pattern matching.
;;;
;;; A binding is an association list from variable names to atoms, and from
;;; glob names to lists of atoms, sorted by name.

(define (variable-form? form)
  (and (pair? form) (eq? 'Variable (car form))
       (pair? (cdr form)) (string? (cadr form))
       (or (null? (cddr form))
           (and (symbol? (caddr form)) (null? (cdddr form))))))

(define (glob-form? form)
  (and (pair? form) (eq? 'Glob (car form))
       (pair? (cdr form)) (string? (cadr form)) (null? (cddr form))))

(define (check-pattern form)
  (cond ((or (variable-form? form) (node-form? form)) #t)
        ((glob-form? form)
         (error "match: a glob may only be the last element of a link:" form))
        ((link-form? form)
         (let loop ((forms (cdr form)))
           (cond ((null? forms) #t)
                 ((and (glob-form? (car forms)) (null? (cdr forms))) #t)
                 (else (check-pattern (car forms)) (loop (cdr forms))))))
        (else (error "match: not a pattern:" form))))

(define (binding-ref binding name)
  "What NAME is bound to in BINDING, or #f."
  (assoc-ref binding name))

(define (bind binding name value same?)
  ;; BINDING extended with NAME -> VALUE, or #f if NAME is bound otherwise.
  (let ((entry (assoc name binding)))
    (cond ((not entry)
           (sort (cons (cons name value) binding)
                 (lambda (a b) (string<? (car a) (car b)))))
          ((same? (cdr entry) value) binding)
          (else #f))))

(define (same-atoms? a b)
  (and (list? a) (list? b) (= (length a) (length b)) (every eq? a b)))

(define (match-atom space pattern atom binding)
  ;; -> the list of bindings extending BINDING under which PATTERN is ATOM.
  (cond
   ((variable-form? pattern)
    (if (or (null? (cddr pattern)) (eq? (caddr pattern) (atom-type atom)))
        (let ((extended (bind binding (cadr pattern) atom eq?)))
          (if extended (list extended) '()))
        '()))
   ((node-form? pattern)
    (if (and (node? atom)
             (eq? (car pattern) (atom-type atom))
             (string=? (cadr pattern) (atom-name atom)))
        (list binding)
        '()))
   ((and (link? atom) (eq? (car pattern) (atom-type atom)))
    (let* ((elements (cdr pattern))
           (glob (and (pair? elements) (glob-form? (last elements))
                      (cadr (last elements))))
           (fixed (if glob (drop-right elements 1) elements)))
      (if (unordered-type? space (atom-type atom))
          (match-multiset space fixed glob (atom-outgoing atom) binding)
          (match-sequence space fixed glob (atom-outgoing atom) binding))))
   (else '())))

(define (match-rest glob atoms binding)
  ;; The atoms left over: none are allowed unless a glob takes them.
  (cond (glob
         (let ((extended (bind binding glob atoms same-atoms?)))
           (if extended (list extended) '())))
        ((null? atoms) (list binding))
        (else '())))

(define (match-sequence space patterns glob atoms binding)
  (cond ((null? patterns) (match-rest glob atoms binding))
        ((null? atoms) '())
        (else
         (append-map (lambda (extended)
                       (match-sequence space (cdr patterns) glob (cdr atoms)
                                       extended))
                     (match-atom space (car patterns) (car atoms) binding)))))

(define (match-multiset space patterns glob atoms binding)
  ;; Each pattern takes a different element of ATOMS, in every possible way.
  (if (null? patterns)
      (match-rest glob atoms binding)
      (append-map
       (lambda (i)
         (let ((others (append (take atoms i) (drop atoms (+ i 1)))))
           (append-map (lambda (extended)
                         (match-multiset space (cdr patterns) glob others
                                         extended))
                       (match-atom space (car patterns) (list-ref atoms i)
                                   binding))))
       (iota (length atoms)))))

(define (candidates space pattern)
  (cond ((variable-form? pattern)
         (if (null? (cddr pattern))
             (atomspace-atoms space)
             (atoms-of-type space (caddr pattern))))
        ((node-form? pattern)
         (let ((node (find-node space (car pattern) (cadr pattern))))
           (if node (list node) '())))
        (else (atoms-of-type space (car pattern)))))

(define (match-under space pattern binding)
  (append-map (lambda (atom) (match-atom space pattern atom binding))
              (candidates space pattern)))

(define (match-all space patterns)
  "The bindings under which every pattern in the list PATTERNS is an atom
of SPACE.  Variables shared between patterns must agree."
  (for-each check-pattern patterns)
  (delete-duplicates
   (fold (lambda (pattern bindings)
           (append-map (lambda (binding) (match-under space pattern binding))
                       bindings))
         '(())
         patterns)))

(define (match-pattern space pattern)
  "The bindings under which PATTERN is an atom of SPACE."
  (match-all space (list pattern)))

(define (instantiate! space template binding)
  "Add to SPACE the atom that TEMPLATE becomes when its variables and globs
are replaced by what BINDING gives them."
  (define (fill form)
    ;; -> the list of atoms FORM stands for: one, or a glob's worth.
    (cond ((variable-form? form)
           (list (or (binding-ref binding (cadr form))
                     (error "instantiate!: unbound variable:" (cadr form)))))
          ((glob-form? form)
           (let ((entry (assoc (cadr form) binding)))
             (unless entry
               (error "instantiate!: unbound glob:" (cadr form)))
             (cdr entry)))
          ((node-form? form) (list (add-node! space (car form) (cadr form))))
          ((link-form? form)
           (list (add-link! space (car form)
                            (let loop ((forms (cdr form)) (acc '()))
                              (if (null? forms)
                                  (concatenate (reverse acc))
                                  (loop (cdr forms)
                                        (cons (fill (car forms)) acc)))))))
          (else (error "instantiate!: not a template:" form))))
  (when (glob-form? template)
    (error "instantiate!: a glob may only appear inside a link:" template))
  (car (fill template)))

;;; Trees as nested links, keyed by Matula number.
;;;
;;; A tree is the unordered link (Tree child ...) over the atoms of its root
;;; subtrees; the single node is the empty link (Tree).  Because atoms are
;;; unique, equal subtrees are one atom, and the store holds each distinct
;;; tree once however often it occurs.  Each tree atom t is keyed by
;;; (Matula (Number "m") t), m being its Matula number.

(define (tree-atom? atom)
  (and (link? atom) (eq? 'Tree (atom-type atom))))

(define (add-tree! space tree)
  "Add TREE and all of its subtrees to SPACE; return the atom of TREE.  It
is an error if the Matula number of TREE exceeds (matula-element-limit)."
  (unless (tree? tree)
    (error "add-tree!: not a tree:" tree))
  (unless (unordered-type? space 'Tree)
    (error "add-tree!: Tree is not an unordered type of this store"))
  ;; Checked first, without computing the number: it can be far too large
  ;; to compute at all.  The subtrees have smaller numbers.
  (unless (tree->matula/bounded tree)
    (error "add-tree!: the Matula number of the tree exceeds the limit:"
           (matula-element-limit)))
  (car
   (let add ((tree tree))
     ;; -> (atom . Matula number)
     (let* ((children (let loop ((subtrees tree) (acc '()))
                        (if (null? subtrees)
                            (reverse acc)
                            (loop (cdr subtrees) (cons (add (car subtrees)) acc)))))
            (number (fold (lambda (child product)
                            (* product (nth-prime (cdr child))))
                          1
                          children))
            (atom (add-link! space 'Tree (map car children))))
       (add-link! space 'Matula
                  (list (add-node! space 'Number (number->string number)) atom))
       (cons atom number)))))

(define (tree-atom space number)
  "The tree atom of SPACE keyed by the Matula number NUMBER, or #f."
  (let ((key (find-node space 'Number (number->string number))))
    (and key
         (let ((entry (find (lambda (link)
                              (and (eq? 'Matula (atom-type link))
                                   (eq? key (car (atom-outgoing link)))))
                            (atom-incoming key))))
           (and entry (cadr (atom-outgoing entry)))))))

(define (atom->tree atom)
  "The canonical tree stored as the tree atom ATOM."
  (unless (tree-atom? atom)
    (error "atom->tree: not a tree atom:" atom))
  (tree-canonical
   (let build ((atom atom))
     (map (lambda (child)
            (unless (tree-atom? child)
              (error "atom->tree: not a tree atom:" (atom->sexp child)))
            (build child))
          (atom-outgoing atom)))))

(define (atom->matula atom)
  "The Matula number that keys the tree atom ATOM, or #f if it has no key."
  (let ((entry (find (lambda (link)
                       (and (eq? 'Matula (atom-type link))
                            (eq? atom (cadr (atom-outgoing link)))))
                     (atom-incoming atom))))
    (and entry (string->number (atom-name (car (atom-outgoing entry)))))))

;;; Trees as hypergraphs on their nodes.
;;;
;;; For a rooted parent map f on the points 0 .. n-1, point i gives the
;;; hyperedge (Attention (Position "i") (Set ...)) whose set holds i and all
;;; of its ancestors.  A map between two trees that sends root to root and
;;; parent to parent carries each hyperedge of the first onto a hyperedge
;;; of the second.

(define (position-node space i)
  (add-node! space 'Position (number->string i)))

(define (add-ancestor-hyperedges! space f)
  "Add the hyperedges of the rooted parent map F to SPACE; return them as a
list, the hyperedge of point i at index i."
  (unless (and (vector? f) (rooted-parent-map? f))
    (error "add-ancestor-hyperedges!: not a rooted parent map:" f))
  (map (lambda (i)
         (add-link! space 'Attention
                    (list (position-node space i)
                          (add-link! space 'Set
                                     (let climb ((j i) (acc '()))
                                       (let ((acc (cons (position-node space j) acc))
                                             (p (vector-ref f j)))
                                         (if (= p j) acc (climb p acc))))))))
       (iota (vector-length f))))

(define (ancestor-hyperedges->parent-map edges)
  "The rooted parent map whose hyperedges are EDGES, in any order."
  (define (point atom) (string->number (atom-name atom)))
  (let* ((n (length edges))
         (f (make-vector n #f))
         (sets (make-vector n #f)))
    (for-each
     (lambda (edge)
       (unless (and (link? edge) (eq? 'Attention (atom-type edge))
                    (= 2 (atom-arity edge)))
         (error "ancestor-hyperedges->parent-map: not a hyperedge:" edge))
       (let ((i (point (car (atom-outgoing edge)))))
         (unless (and (integer? i) (< -1 i n) (not (vector-ref sets i)))
           (error "ancestor-hyperedges->parent-map: the queries must be the points 0 .. n-1 once each"))
         (vector-set! sets i (map point (atom-outgoing (cadr (atom-outgoing edge)))))))
     edges)
    ;; The parent of i is the ancestor whose own set is the set of i less i.
    (for-each
     (lambda (i)
       (let* ((others (delv i (vector-ref sets i)))
              (parent (find (lambda (j)
                              (= (length (vector-ref sets j)) (length others)))
                            others)))
         (vector-set! f i (cond ((null? others) i)
                                (parent parent)
                                (else (error "ancestor-hyperedges->parent-map: the hyperedges are not those of a tree"))))))
     (iota n))
    (unless (rooted-parent-map? f)
      (error "ancestor-hyperedges->parent-map: the hyperedges are not those of a tree"))
    f))
