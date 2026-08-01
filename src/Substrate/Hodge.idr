module Substrate.Hodge

import Substrate.Core
import Substrate.Laplacian
import Math.Multiset
import Math.IntPolynumber
import Data.List
import Math.BoxInt

%default total

public export
record HodgeComponents where
  constructor MkHodge
  harmonic : Vexel
  gradient : Vexel
  curl     : Vexel

public export
innerProductPoly : IntPolynumber -> IntPolynumber -> Integer
innerProductPoly p1 p2 =
  let list1 = multisetToList p1
      list2 = multisetToList p2
      coeff2 : (Nat, Nat) -> Integer
      coeff2 exp =
        case filter (\(e, _) => e == exp) list2 of
          ((_, c) :: _) =>
            let (MkUr val) = boxToInt c
            in val
          []            => 0
  in sum (map (\(exp, c) =>
        let (MkUr val) = boxToInt c
        in val * coeff2 exp) list1)

public export
innerProductVexel : Vexel -> Vexel -> Integer
innerProductVexel m1 m2 =
  let list1 = multisetToList m1
      list2 = multisetToList m2
      amp2 : Geometry -> Amplitude
      amp2 geom =
        case filter (\((g, _), _) => g == geom) list2 of
          (((_, amp), _) :: _) => amp
          []                   => emptyAmplitude
  in sum (map (\((geom, amp), _) => innerProductPoly amp (amp2 geom)) list1)

public export
subtractFields : Vexel -> Vexel -> Vexel
subtractFields f1 f2 = subMultiset f1 f2

public export
invertLaplacianExact : Vexel -> Substrate -> Vexel
invertLaplacianExact divergence substrate =
  let nodes = substrateNodes substrate
      trialPotentials = map (\n => singletonVexel n (posTerm 0 0 1)) nodes
      
      stepProjection : Vexel -> Vexel -> Vexel
      stepProjection acc potential =
        let deltaPot   = multisetLaplacian potential substrate
            num        = innerProductVexel divergence deltaPot
            den        = innerProductVexel deltaPot deltaPot
        in if den == 0 
              then acc 
              else let scaleFactor = num `div` den
                   in addMultiset (scaleMultiset scaleFactor potential) acc
  in foldl stepProjection ZeroM trialPotentials

public export
hodgeDecompose : Vexel -> Substrate -> HodgeComponents
hodgeDecompose field substrate =
  let laplacianField = multisetLaplacian field substrate
  in if multiplicityAll laplacianField == 0
       then MkHodge field ZeroM ZeroM
       else
         let potential     = invertLaplacianExact laplacianField substrate
             gradientField = multisetLaplacian potential substrate
             curlField     = subtractFields field gradientField
         in MkHodge ZeroM gradientField curlField
