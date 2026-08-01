module Substrate.Divergence

import Substrate.Core
import Math.Multiset

%default total

||| Evaluates node net charge divergence from incoming/outgoing Pixel link multiplicities.
||| Maps a 1-chain Substrate to a 0-chain Vexel.
public export
applyDivergenceMap : Substrate -> Vexel
applyDivergenceMap sub =
  let edges = multisetToList sub
      boundaryList = computeBoundaryIndex edges
      states = map (\(geom, count) => ((geom, emptyIntPoly), count)) boundaryList
  in fromList states
  where
    computeBoundaryIndex : List ((Geometry, Geometry), Integer) -> List (Geometry, Integer)
    computeBoundaryIndex [] = []
    computeBoundaryIndex (((src, tgt), count) :: xs) =
      (tgt, count) :: (src, -count) :: computeBoundaryIndex xs

||| Alias for divergence map.
public export
boundaryOp : Substrate -> Vexel
boundaryOp = applyDivergenceMap
