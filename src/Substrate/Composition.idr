module Substrate.Composition

import public Substrate.Core
import public Substrate.Twist
import public Math.Chromogeometry
import public Math.Multiset
import public Data.List

%default total

-----------------------------------------------------------------------
-- COMPOSITE STATE (Generic Platonic Field Configuration)
-----------------------------------------------------------------------

||| A CompositeState represents a pure, stateless mathematical field topology —
||| a spatial state vector (0-Cochain) mapping coordinates to amplitudes.
public export
0 CompositeState : Type
CompositeState = Vexel

||| Translates a CompositeState's coordinates by a displacement vector.
public export
translateState : Geometry -> CompositeState -> CompositeState
translateState (MkPixel dx dy) m =
  let stateItems = multisetToList m
      translatedState = map (\((MkPixel x y, amp), count) => 
                              ((MkPixel (x + dx) (y + dy), amp), count)
                            ) stateItems
  in fromList translatedState

||| Computes the composite rational spread of the active coordinates in the state.
public export
computeStateSpread : Metric -> CompositeState -> (Integer, Integer)
computeStateSpread metric m =
  let coords = map (fst . fst) (multisetToList m)
      _ = metric -- Ignored by the Three-Fold Invariant
      -- Keep only unique coordinates (active nodes)
      uniqueCoords = nub coords
      -- Extract all triads of distinct coordinates (p1, p2, p3)
      triads = [ (p1, p2, p3)
               | p1 <- uniqueCoords
               , p2 <- uniqueCoords
               , p3 <- uniqueCoords
               , p1 /= p2, p2 /= p3, p3 /= p1
               ]
      
      -- Helper function to destructure the triad and compute its spread (Blue unconditionally)
      getSpread : (Geometry, Geometry, Geometry) -> (Integer, Integer)
      getSpread (p1, p2, p3) =
        let (num, den) = spreadNL Blue p1 p2 p3
            (MkUr n) = boxToInt num
            (MkUr d) = boxToInt den
        in (n, d)
      
      -- Map triads to exact rational spreads
      spreadFractions = map getSpread triads
  in foldl addRationalLocal (0, 1) spreadFractions
