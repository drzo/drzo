# Status

What exists for each of the 17 formulas in [README.md](README.md), and how it
is checked. "Tested" means a SRFI-64 suite under `tests/` that runs in CI with
`make check`. "Placeholder" means code exists in the original top-level
scripts but computes stand-in values, not the mathematics named.

## Formulas

| # | Item | State | Where | Checked by |
|---|------|-------|-------|------------|
| 1 | Sequence `a_n` | Implemented, tested | `ghost/a000081.scm` | 31 terms against the OEIS b-file fixture |
| 2 | `A(x) = x·exp(Σ A(x^k)/k)` | Implemented, tested | `ghost/series.scm`, `ghost/a000081.scm` | Exact coefficient comparison to `x^60` |
| 3 | Recurrence | Implemented, tested | `ghost/a000081.scm` | Recurrence re-evaluated from the definition for `n ≤ 80` |
| 4 | Asymptotics `C·α^n·n^(-3/2)` | Implemented, tested | `ghost/a000081.scm` | `α` and `C` derived from `A(ρ) = 1` and matched to published digits; ratio monotone on 50..500 |
| 5 | Tree sum and product formula | Implemented, tested (corrected form) | `ghost/tree.scm`, `ghost/a000081.scm` | `x·∏(1-x^k)^(-a_k)` to `x^60`; tree counts equal `a_n` for `n ≤ 18` |
| 6 | Trees and endofunctions | Implemented, tested | `ghost/tree.scm` | Brute force over all `n^n` maps for `n ≤ 6` |
| 7 | Partition structure | Implemented, tested (corrected form: a surjection) | `ghost/tree.scm` | Fibre sizes for `n ≤ 12` |
| 8 | Hypergraph functor | Not started | | |
| 9 | B-series | Partly implemented, tested | `ghost/butcher.scm` | Elementary weights and order conditions; elementary differentials `F(τ)` not started |
| 10 | J-surfaces | Placeholder | `advanced-structures.scm` | |
| 11 | P-systems | Placeholder | `advanced-structures.scm` | |
| 12 | Incidence structures | Placeholder | `advanced-structures.scm` | |
| 13 | Block codes | Placeholder | `advanced-structures.scm` | |
| 14 | Orbifolds | Placeholder | `advanced-structures.scm` | |
| 15 | HyperNN | Placeholder | `advanced-structures.scm` | |
| 16 | Meta-pattern / Yoneda | Placeholder | `advanced-structures.scm` | |
| 17 | Topos functor | Placeholder | `advanced-structures.scm` | |

## Other components

| Component | State | Notes |
|-----------|-------|-------|
| Matula–Göbel numbers | Implemented, tested | `ghost/matula.scm`. Round trip for every tree with `n ≤ 12` and every integer up to 5000. |
| Tensor shapes from trees | Implemented, tested | One axis per root subtree, of length `p_{M(subtree)}`. No size cap is applied; see "Open decisions". |
| GPT-2 hypergraph | Placeholder | `gpt2-hypergraph.scm`. The forward pass returns random numbers. |
| TOML writer | Placeholder | `a000081-toml.scm`. Output is now deterministic, but the section-index, ordering, subsection-name and array-as-string bugs remain. There is no parser. |
| Lie-group B-series, simplicial homology | Placeholder | `enhanced-advanced-structures.scm`. Values are fabricated. |

## Plan phases

| Phase | State |
|-------|-------|
| 0 Reproduce and stabilise | Done |
| 1 Exact core | Done |
| 2 Rooted trees, Matula numbers | Done |
| 3 B-series, J-surfaces, P-systems | Order conditions done; elementary differentials, J-surfaces and P-systems not started |
| 4 Hypergraph store | Not started |
| 5 Tensors and GPT-2 forward pass | Not started |
| 6 TOML catalogue | Not started (only `creation_time` removed) |
| 7 Abstract structures | Not started |
| 8 ggml | Not started |

## Phase 0 findings

Every script was run under Guile 3.0.9 before any change. The defects listed
in the plan were confirmed, with one addition:

- `unified-demo.scm` crashed with `error in format` at "Asymptotic: a_n ~ C…"
  because of an unescaped `~`. It did not run to completion.

Changes made to the original scripts: each has a `main` entry point and no
longer runs its demo when loaded; `creation_time` is no longer written;
the definitions that shadowed `quotient` and `random:normal` are renamed;
the format string above is fixed. Nothing else in them was changed, so the
1000-slot cache and the placeholder computations are still there.

## Open decisions

- **Tensor shape cap.** `matula-shape` returns the exact prime-factor shape.
  The element count equals the Matula number, which reaches 50 728 129 at 12
  nodes. Phase 5 needs either a cap or a different axis rule.
- **Readings for items 8 and 12–17.** The plan proposes concrete meanings;
  none is implemented.
- **GPT-2 reference weights**, **ggml binding**, and **agent-zero / bolt.diy
  integration** are untouched.
