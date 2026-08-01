module Substrate.Linear

import Substrate.Core

%default total

public export
0 Edge : Type
Edge = (Substrate.Core.Geometry, Substrate.Core.Geometry)

public export
0 LVexel : (contents : List (Substrate.Core.Geometry, Integer)) -> Type
LVexel contents = LMultiset Integer Substrate.Core.Geometry contents

public export
0 LDepVexel : (contents : List (Substrate.Core.Geometry, Integer)) -> Type
LDepVexel = LVexel

public export
0 LSubstrate : (contents : List (Edge, Integer)) -> Type
LSubstrate contents = LMultiset Integer Edge contents

public export
0 LDepSubstrate : (contents : List (Edge, Integer)) -> Type
LDepSubstrate = LSubstrate

public export
computeBoundaryIndex : List (Edge, Integer) -> List (Substrate.Core.Geometry, Integer)
computeBoundaryIndex [] = []
computeBoundaryIndex (((src, tgt), count) :: xs) = 
  (tgt, count) :: (src, -count) :: computeBoundaryIndex xs

public export
nextContents : List (a, Integer) -> List (a, Integer)
nextContents [] = []
nextContents ((item, count) :: xs) = (item, count + 1) :: nextContents xs

public export
applyBoundary : {0 edges : List (Edge, Integer)} -> 
                (1 chain : LDepSubstrate edges) -> 
                LDepVexel (computeBoundaryIndex edges)
applyBoundary LEmptyM = LEmptyM
applyBoundary (LAddM (src, tgt) count prev) =
  LAddM tgt count (LAddM src (-count) (applyBoundary prev))

public export
stepUniverse : {0 contents : List (a, Integer)} -> 
                (1 currentMesh : LMultiset Integer a contents) -> 
                LMultiset Integer a (nextContents contents)
stepUniverse LEmptyM = LEmptyM
stepUniverse (LAddM item count prev) = LAddM item (count + 1) (stepUniverse prev)

public export
0 DynamicVexel : Type
DynamicVexel = (c : List (Substrate.Core.Geometry, Integer) ** LDepVexel c)

public export
0 DynamicSubstrate : Type
DynamicSubstrate = (edges : List (Edge, Integer) ** LDepSubstrate edges)

public export
runBoundary : DynamicSubstrate -> DynamicVexel
runBoundary (edges ** chain) = (computeBoundaryIndex edges ** applyBoundary chain)

public export
0 DynamicUniverse : (a : Type) -> Type
DynamicUniverse a = (c : List (a, Integer) ** LMultiset Integer a c)

public export
runDynamicEpoch : DynamicUniverse a -> DynamicUniverse a
runDynamicEpoch (c ** mesh) = (nextContents c ** stepUniverse mesh)

public export
data LUniverseState : (edges : List (Edge, Integer)) -> 
                      (contents : List ((Substrate.Core.Geometry, Amplitude), Integer)) -> 
                      Type where
  MkLUniverseState : (1 substrate : LSubstrate edges) -> 
                     (1 stateVector : LMultiset Integer (Substrate.Core.Geometry, Amplitude) contents) -> 
                     LUniverseState edges contents

public export
0 DynamicLUniverseState : Type
DynamicLUniverseState = 
  (edges : List (Edge, Integer) ** 
   contents : List ((Substrate.Core.Geometry, Amplitude), Integer) ** 
   LUniverseState edges contents)

public export
sumLinearAmplitudes : List ((Substrate.Core.Geometry, Amplitude), Integer) -> Amplitude
sumLinearAmplitudes [] = emptyIntPoly
sumLinearAmplitudes (((_, poly), count) :: xs) = 
  addIntPoly (scaleMultiset (fromInteger count) poly) (sumLinearAmplitudes xs)

public export
computeAscendContents : (target : Substrate.Core.Geometry) -> 
                         List ((Substrate.Core.Geometry, Amplitude), Integer) -> 
                         List ((Substrate.Core.Geometry, Amplitude), Integer)
computeAscendContents target contents =
  let sumPoly = sumLinearAmplitudes contents
  in [((target, sumPoly), 1)]

public export
lascendScale : {0 contents : List ((Substrate.Core.Geometry, Amplitude), Integer)} -> 
               (target : Substrate.Core.Geometry) -> 
               (1 state : LMultiset Integer (Substrate.Core.Geometry, Amplitude) contents) -> 
               LMultiset Integer (Substrate.Core.Geometry, Amplitude) (computeAscendContents target contents)
lascendScale target state =
  let MkLUnboxResult frozen = lunboxLMultiset state
  in LAddM (target, sumLinearAmplitudes frozen) 1 LEmptyM
