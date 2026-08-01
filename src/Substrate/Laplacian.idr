module Substrate.Laplacian

import Substrate.Core
import Substrate.Difference
import Substrate.Divergence

%default total

||| Evaluates Δ = Divergence ∘ Difference map over multiset fields.
public export
multisetLaplacian : Vexel -> Substrate -> Vexel
multisetLaplacian field substrate =
  let gradient = applyDifferenceMap field substrate
  in applyDivergenceMap gradient

||| Alias for discrete Laplacian.
public export
discreteLaplacian : Vexel -> Substrate -> Vexel
discreteLaplacian = multisetLaplacian
