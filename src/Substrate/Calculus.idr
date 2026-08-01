module Substrate.Calculus

import Math.IntPolynumber
import Math.SpreadPolynumber
import Math.Multiset
import Data.List
import Substrate.Core
import Math.BoxInt

%default total

public export
record DiracHole a where
  constructor MkHole
  leftContext  : List a
  rightContext : List a

public export
mcbrideDerivative : List a -> Maybe (DiracHole a, a)
mcbrideDerivative [] = Nothing
mcbrideDerivative (x :: xs) = Just (MkHole [] xs, x)

public export covering
wildbergerDerivative : Nat -> IntPolynumber
wildbergerDerivative n = 
  let sn  = spreadPoly n
      sn1 = spreadPoly (S n)
  in subIntPoly sn1 sn

public export
leibnizIntegralLag : Nat -> Nat
leibnizIntegralLag Z = 0
leibnizIntegralLag (S k) = 
  (S k) + leibnizIntegralLag k

public export
substrateIntegral : Substrate -> Vexel -> BoxInt
substrateIntegral substrate field =
  let edges  = multisetToList substrate
      states = multisetToList field
      
      getEnergy : Substrate.Core.Geometry -> BoxInt
      getEnergy geom =
        case filter (\((g, _), _) => g == geom) states of
          (((_, amp), _) :: _) => multiplicityAll amp
          []                   => 0
          
      edgeContribution : ((Substrate.Core.Geometry, Substrate.Core.Geometry), Integer) -> BoxInt
      edgeContribution ((src, tgt), count) =
        let energySrc = getEnergy src
            energyTgt = getEnergy tgt
        in fromInteger count * (energySrc + energyTgt)
  in sum (map edgeContribution edges)

public export
discreteFluxIntegral : Substrate -> BoxInt
discreteFluxIntegral sub =
  sum (map (fromInteger . snd) (multisetToList sub))

public export
wildbergerAntiderivative : List IntPolynumber -> List IntPolynumber
wildbergerAntiderivative dfs =
  go ZeroM dfs
  where
    go : IntPolynumber -> List IntPolynumber -> List IntPolynumber
    go acc [] = [acc]
    go acc (df :: rest) =
      acc :: go (addIntPoly acc df) rest
