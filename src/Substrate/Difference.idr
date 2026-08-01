module Substrate.Difference

import Substrate.Core

%default total

||| Evaluates node potential differences across directed Pixel links.
||| Projects a 0-cochain Vexel to a 1-cochain Substrate.
public export
applyDifferenceMap : Vexel -> Substrate -> Substrate
applyDifferenceMap stateVector substrate =
  let edges  = multisetToList substrate
      states = multisetToList stateVector
      
      getEnergy : Geometry -> Integer
      getEnergy geom =
        case filter (\((g, _), _) => g == geom) states of
          ((_, c) :: _) => c
          []            => 0
          
      differenceEdges = map (\((src, tgt), count) =>
                              let energySrc = getEnergy src
                                  energyTgt = getEnergy tgt
                                  diff = energyTgt - energySrc
                              in ((src, tgt), diff)
                            ) edges
  in fromList differenceEdges

||| Alias for difference map.
public export
applyCoboundary : Vexel -> Substrate -> Substrate
applyCoboundary = applyDifferenceMap
