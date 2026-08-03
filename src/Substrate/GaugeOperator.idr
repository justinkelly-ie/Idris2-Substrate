module Substrate.GaugeOperator

import Substrate.Core
import Math.Multiset
import Math.Pixel
import Data.List
import Math.BoxInt

%default total

||| A Maxel operator acting as a discrete transition matrix over Pixel coordinates
public export
0 MaxelOperator : Type
MaxelOperator = Multiset Integer (Geometry, Geometry)

||| Enforces that an operator preserves total system energy/tally (Isometry constraint)
public export
isPermutationMaxel : MaxelOperator -> Bool
isPermutationMaxel op = 
  let list = multisetToList op
      sources = map (fst . fst) list
      targets = map (snd . fst) list
      weights = map snd list
      
      -- Every transition must map with a magnitude of 1 or -1
      weightsOk = all (\w => w == 1 || w == -1) weights
      
      isUnique : Eq a => List a -> Bool
      isUnique [] = True
      isUnique (x :: xs) = if elem x xs then False else isUnique xs
  in weightsOk && isUnique sources && isUnique targets

||| Applies a Gauge Maxel as a coordinate-permuting force operator.
||| Translates amplitude values across the discrete pixel graph based on edge transitions.
public export
applyGaugeMaxel : MaxelOperator -> Vexel -> Vexel
applyGaugeMaxel op state =
  let stateList = multisetToList state
      opList    = multisetToList op
      
      applyNode : ((Geometry, Amplitude), Integer) -> List ((Geometry, Amplitude), Integer)
      applyNode ((geom, amp), count) =
        case filter (\((src, _), _) => src == geom) opList of
          (((src, tgt), weight) :: _) => 
            [((tgt, scaleMultiset (intToBoxInt weight) amp), count)]
          [] => 
            [((geom, amp), count)]
            
      mappedList = concatMap applyNode stateList
  in fromList mappedList
