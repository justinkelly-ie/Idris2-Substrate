module Substrate.Core

import public Math.Multiset
import public Math.LMultiset
import public Math.Maxel
import public Math.DepMultiset
import public Math.DepMaxel
import public Math.Vexel.DepVexel
import public Math.IntPolynumber
import public Math.Chromogeometry
import public Math.Pixel
import public Math.BoxInt
import public Math.Interfaces
import Data.List

%default total

-----------------------------------------------------------------------
-- 1. GEOMETRY (The Elementary Spatial Unit / 1-Cell)
-----------------------------------------------------------------------

||| A chromogeometric coordinate cell — the elementary spatial unit.
public export
0 Geometry : Type
Geometry = Pixel Blue BoxInt

-----------------------------------------------------------------------
-- 2. AMPLITUDE (The Quantum State Value / Polynomial Coefficient)
-----------------------------------------------------------------------

||| The polynomial amplitude at a geometric coordinate.
public export
0 Amplitude : Type
Amplitude = IntPolynumber

public export
emptyAmplitude : Amplitude
emptyAmplitude = ZeroM

-----------------------------------------------------------------------
-- 3. SUBSTRATE (The Causal Graph — pure Multiset of directed edges)
-----------------------------------------------------------------------

||| The directed causal relations of the spacetime grid (causal poset).
||| Spacetime relations are a multiset of pixel-to-pixel edges.
public export
0 Substrate : Type
Substrate = Multiset Integer (Geometry, Geometry)

-----------------------------------------------------------------------
-- 4. VEXEL (The State Vector / 0-Cochain)
-----------------------------------------------------------------------

||| The state vector (energy/mass field coefficients).
||| A state is a multiset of pixel-amplitude pairs.
public export
0 Vexel : Type
Vexel = Multiset Integer (Geometry, Amplitude)

||| The total Leibniz Lag of a Vexel (sum of all entry multiplicities).
public export
stateLag : Vexel -> Integer
stateLag m = multiplicityAll m

-----------------------------------------------------------------------
-- 5. UNIVERSE STATE (The Total Configuration)
-----------------------------------------------------------------------

||| The complete configuration: a causal substrate graph paired with a
||| state vector.
public export
record UniverseState where
  constructor MkUniverseState
  substrate    : Substrate
  stateVector  : Vexel

-----------------------------------------------------------------------
-- 6. SUBSTRATE UTILITIES
-----------------------------------------------------------------------

||| Computes the total causal density (Leibniz Lag) of the Substrate.
public export
substrateLag : Substrate -> Nat
substrateLag sub = Prelude.integerToNat (multiplicityAll sub)

||| Merge two Substrate causal graphs (native multiset union).
public export
mergeSubstrate : Substrate -> Substrate -> Substrate
mergeSubstrate = addMultiset

||| The empty Substrate (vacuum — no causal relations).
public export
emptySubstrate : Substrate
emptySubstrate = ZeroM

||| A single directed causal edge: g1 causally precedes g2.
public export
singleEdge : Geometry -> Geometry -> Substrate
singleEdge g1 g2 = fromList [((g1, g2), 1)]

||| Extracts all unique Geometry nodes referenced in the Substrate.
public export
substrateNodes : Substrate -> List Geometry
substrateNodes sub =
  nub (concatMap (\((g1, g2), _) => [g1, g2]) (multisetToList sub))

-----------------------------------------------------------------------
-- 7. VEXEL UTILITIES
-----------------------------------------------------------------------

||| The empty Vexel — the physical vacuum state.
public export
emptyVexel : Vexel
emptyVexel = ZeroM

||| A singleton Vexel — a single coordinate mapped to one amplitude.
public export
singletonVexel : Geometry -> Amplitude -> Vexel
singletonVexel geom amp = fromList [((geom, amp), 1)]

||| Superposition — the native multiset union of two state vectors.
public export
superposeStates : Vexel -> Vexel -> Vexel
superposeStates = addMultiset

||| Restriction of a Vexel to entries matching a specific Geometry.
public export
restrictToPixel : Geometry -> Vexel -> Vexel
restrictToPixel geom pip =
  fromList (filter (\((g, _), _) => g == geom) (multisetToList pip))

||| Checks that every Geometry referenced in the Vexel exists as a node
||| in the Substrate causal graph.
public export
isSynchronised : Substrate -> Vexel -> Bool
isSynchronised sub pip =
  let subNodes = substrateNodes sub
      pipCoords = map (fst . fst) (multisetToList pip)
  in all (\g => elem g subNodes) pipCoords

-----------------------------------------------------------------------
-- 8. SERIALIZATION BRIDGE (JSON Export)
-----------------------------------------------------------------------

private
join : String -> List String -> String
join sep [] = ""
join sep [x] = x
join sep (x :: xs) = x ++ sep ++ join sep xs

public export
serializeGeometry : Geometry -> String
serializeGeometry (MkPixel s t) =
  let (MkUr sVal) = boxToInt s
      (MkUr tVal) = boxToInt t
  in "{\"src\":" ++ show sVal ++ ",\"tgt\":" ++ show tVal ++ "}"

public export
serializeTerm : ((Nat, Nat), BoxInt) -> String
serializeTerm ((alpha, beta), count) =
  let (MkUr countVal) = boxToInt count
  in "{\"alpha\":" ++ show alpha ++ ",\"beta\":" ++ show beta ++ ",\"count\":" ++ show countVal ++ "}"

public export
serializeAmplitude : Amplitude -> String
serializeAmplitude amp =
  "[" ++ join "," (map serializeTerm (multisetToList amp)) ++ "]"

public export
serializeMaxelItem : ((Geometry, Amplitude), Integer) -> String
serializeMaxelItem ((geom, amp), count) =
  "{\"geom\":" ++ serializeGeometry geom ++
  ",\"amplitude\":" ++ serializeAmplitude amp ++
  ",\"count\":" ++ show count ++ "}"

public export
serializeVexel : Vexel -> String
serializeVexel maxel =
  "[" ++ join "," (map serializeMaxelItem (multisetToList maxel)) ++ "]"

public export
serializeEdge : ((Geometry, Geometry), Integer) -> String
serializeEdge ((parent, child), count) =
  "{\"parent\":" ++ serializeGeometry parent ++
  ",\"child\":" ++ serializeGeometry child ++
  ",\"count\":" ++ show count ++ "}"

public export
serializeSubstrate : Substrate -> String
serializeSubstrate sub =
  "[" ++ join "," (map serializeEdge (multisetToList sub)) ++ "]"

public export
serializeUniverseState : UniverseState -> String
serializeUniverseState (MkUniverseState sub stateVec) =
  "{\"substrate\":" ++ serializeSubstrate sub ++
  ",\"stateVector\":" ++ serializeVexel stateVec ++ "}"
