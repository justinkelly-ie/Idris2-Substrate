# 🕸️ idris2-Substrate

**The Causal Substrate Network & Discrete Multiset Calculus Library in [Idris 2](https://github.com/idris-lang/Idris2).**

[![Idris2](https://img.shields.io/badge/Idris2-Substrate-blue.svg)](https://github.com/idris-lang/Idris2)

---

## 🏛️ Overview

`idris2-Substrate` is the shared topological complex and discrete multiset calculus library powering the Finite Science framework. It decouples generic multiset graph calculus, 0/1-cochains, difference maps, divergence maps, Laplacians, and Hodge decomposition from domain-specific physics engines.

### Key Multiset Architecture

All topological concepts in `idris2-Substrate` are built on top of `idris2-Multiset` and `idris2-Chromogeometry`:

- **Node / 0-Cell**: `BoxInt` / `Geometry` — Vertex coordinate (`BoxInt` scalar dimension or `Geometry = Pixel Blue BoxInt` node point).
- **Edge / 1-Cell**: `(Geometry, Geometry)` — Directed coordinate edge in the causal substrate.
- **Substrate / Causal Graph**: `Multiset Integer (Geometry, Geometry)` — Multiset of 1-cells (directed edges).
- **Vexel / 0-Cochain**: `Multiset Integer (Geometry, Amplitude)` — 0-cochain mapping nodes to state amplitudes.
- **Maxel / Operator**: `Multiset c (Pixel metric node)` — Operator matrix / plaquette loop.

---

## 📁 Module Map

| Module | Description |
|---|---|
| [Substrate.Core](src/Substrate/Core.idr) | Fundamental `Substrate`, `Vexel`, `Geometry`, `Amplitude`, `UniverseState`, and JSON serialization bridge. |
| [Substrate.Difference](src/Substrate/Difference.idr) | `applyDifferenceMap` (Coboundary $d_0$ potential difference over directed Pixels). |
| [Substrate.Divergence](src/Substrate/Divergence.idr) | `applyDivergenceMap` (Boundary $\partial_1$ node net charge divergence). |
| [Substrate.Laplacian](src/Substrate/Laplacian.idr) | `multisetLaplacian` ($\Delta = \text{Divergence} \circ \text{Difference}$ over multisets). |
| [Substrate.Hodge](src/Substrate/Hodge.idr) | `hodgeDecompose`, `invertLaplacianExact`, and inner product projections. |
| [Substrate.Linear](src/Substrate/Linear.idr) | `LVexel`, `LSubstrate`, `applyBoundary`, `lascendScale`, and type-verified linear engine. |
| [Substrate.Calculus](src/Substrate/Calculus.idr) | McBride derivatives, Wildberger derivatives, Leibniz integrals, and path ensembles. |

---

## 📚 References

- **Norman J. Wildberger**: *Divine Proportions* and *Chromogeometry*.
- **Google DeepMind Antigravity Team**: Finite Science Architecture.

© Justin Kelly. All rights reserved.
