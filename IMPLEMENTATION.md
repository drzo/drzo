# Ghost in the Guile Shell - implementation guide

See [STATUS.md](STATUS.md) for what is implemented and what is a placeholder.

## Prerequisites

```bash
sudo apt install guile-3.0
```

## Tests

```bash
make check
```

This runs the SRFI-64 suites in `tests/`. The tree suite enumerates all
1 721 159 trees with 18 nodes, so a full run takes about half a minute.

## Modules

The tested code lives in `ghost/` as Guile modules. Run Guile with the
repository root on the load path:

```bash
guile -L .
```

```scheme
(use-modules (ghost a000081) (ghost tree) (ghost matula) (ghost butcher))

(a000081 10)                        ; 719
(a000081-list 8)                    ; (0 1 1 2 4 9 20 48)
(a000081-alpha)                     ; 2.955765285651995

(trees-of-size 4)                   ; ((() () ()) ((()) ()) ((() ())) (((()))))
(tree-symmetry '(() () ()))         ; 6
(tree-density '((()) ()))           ; 8
(parent-map->tree #(2 2 2 0))       ; ((()) ())

(tree->matula '((()) ()))           ; 6
(matula->tree 6)                    ; ((()) ())
(matula-shape '((()) ()))           ; (2 3)

(rk-order (rk4-tableau))            ; 4
(elementary-weight (rk4-tableau) '(() ()))   ; 1/3
```

| Module | Contents |
|--------|----------|
| `(ghost series)` | Truncated power series over the exact rationals: add, multiply, shift, `A(x^k)`, `exp`, `log`, `(1-x^k)^(-m)` |
| `(ghost a000081)` | The sequence with a growable cache, its generating function, `ρ`, `α` and Otter's constant |
| `(ghost tree)` | Canonical rooted trees, enumeration by size, `σ`, `γ`, height, labelled trees as parent maps, the map to partitions |
| `(ghost matula)` | Primes, Matula–Göbel numbers in both directions, tensor shapes |
| `(ghost butcher)` | Butcher tableaux, elementary weights, Runge–Kutta order conditions |

A tree is the list of its root's subtrees: `()` is the single node and
`((()) ())` is a root with a two-node chain and a leaf below it.

## Command line

```bash
bin/ghost seq 12         # first 12 terms
bin/ghost trees 5        # the 9 trees with 5 nodes and their invariants
bin/ghost matula 360     # the tree numbered 360
bin/ghost order rk4      # order of a Runge-Kutta method
bin/ghost constants      # alpha and C
make demo                # all of the above
```

## Original scripts

The top-level `*.scm` files are the original demonstration scripts. Loading
one defines its procedures and prints nothing. To run a demo, call its
`main`:

```bash
guile -e main -s simple-demo.scm
guile -e main -s unified-demo.scm
```

`a000081-toml.scm` and `unified-demo.scm` rewrite the two `.toml` files in the
current directory when run this way. The output is deterministic.

Most of what these scripts print for the advanced structures comes from
placeholder code; STATUS.md lists which.
