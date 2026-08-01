module Main

import QuickCheck
import Data.List
import Math.Multiset
import Math.BoxInt
import Math.Pixel
import Math.Chromogeometry
import Math.IntPolynumber
import Math.SpreadPolynumber
import Math.Interfaces
import Substrate.Core
import Substrate.Difference
import Substrate.Divergence
import Substrate.Laplacian
import Substrate.Hodge
import Substrate.Linear
import Substrate.Calculus

%default total

--------------------------------------------------------------------------------
-- 1. ARBITRARY INSTANCES
--------------------------------------------------------------------------------

public export
Arbitrary BoxInt where
  arbitrary = do
    n <- arbitrary {a=Integer}
    pure (fromInteger n)
  coarbitrary b gen =
    let (Math.Interfaces.MkUr val) = boxToInt b
    in coarbitrary val gen

public export
Arbitrary Geometry where
  arbitrary = do
    x <- arbitrary {a=BoxInt}
    y <- arbitrary {a=BoxInt}
    pure (MkPixel x y)
  coarbitrary (MkPixel x y) gen =
    coarbitrary x (coarbitrary y gen)

--------------------------------------------------------------------------------
-- 2. DEFINITION TESTS: CORE SUBSTRATE & VEXEL ALGEBRA
--------------------------------------------------------------------------------

||| Substrate graph merge is commutative: s1 ∪ s2 = s2 ∪ s1
prop_substrateMergeCommutative : Property
prop_substrateMergeCommutative = forAll {a = List (Geometry, Geometry)} {prop = Bool} arbitrary (MkFn (\edges =>
  let s1 = fromList (map (\e => (e, 1)) edges)
      s2 = fromList (map (\e => (e, 2)) edges)
  in mergeSubstrate s1 s2 == mergeSubstrate s2 s1))

||| Empty substrate is identity for graph merge: s ∪ ∅ = s
prop_substrateMergeIdentity : Property
prop_substrateMergeIdentity = forAll {a = List (Geometry, Geometry)} {prop = Bool} arbitrary (MkFn (\edges =>
  let s = fromList (map (\e => (e, 1)) edges)
  in mergeSubstrate s emptySubstrate == s))

||| Vacuum substrate has 0 Lag: substrateLag ∅ = 0
prop_vacuumSubstrateLag : Property
prop_vacuumSubstrateLag = forAll {a = Bool} {prop = Bool} arbitrary (MkFn (\_ =>
  substrateLag emptySubstrate == 0))

||| Single edge creation builds 1-edge substrate graph
prop_singleEdgeLag : Property
prop_singleEdgeLag = forAll {a = (Geometry, Geometry)} {prop = Bool} arbitrary (MkFn (\(g1, g2) =>
  let sub = singleEdge g1 g2
  in substrateLag sub == 1))

||| State vector superposition is commutative: v1 + v2 = v2 + v1
prop_superposeCommutative : Property
prop_superposeCommutative = forAll {a = Geometry} {prop = Bool} arbitrary (MkFn (\g =>
  let v1 = singletonVexel g (posTerm 0 0 1)
      v2 = singletonVexel g (posTerm 1 0 2)
  in superposeStates v1 v2 == superposeStates v2 v1))

||| Empty vexel is identity for superposition: v + ∅ = v
prop_superposeIdentity : Property
prop_superposeIdentity = forAll {a = Geometry} {prop = Bool} arbitrary (MkFn (\g =>
  let v = singletonVexel g (posTerm 0 0 1)
  in superposeStates v emptyVexel == v))

||| Restriction to non-existent pixel produces empty vexel
prop_restrictToPixelEmpty : Property
prop_restrictToPixelEmpty = forAll {a = Geometry} {prop = Bool} arbitrary (MkFn (\g =>
  restrictToPixel g emptyVexel == emptyVexel))

||| Synchronisation check: empty substrate and empty vexel are synchronised
prop_synchronisedVacuum : Property
prop_synchronisedVacuum = forAll {a = Bool} {prop = Bool} arbitrary (MkFn (\_ =>
  isSynchronised emptySubstrate emptyVexel == True))

--------------------------------------------------------------------------------
-- 3. PROPERTY TESTS: DIFFERENCE, DIVERGENCE, LAPLACIAN & HODGE
--------------------------------------------------------------------------------

||| Difference map on vacuum produces vacuum 1-cochain
prop_differenceVacuum : Property
prop_differenceVacuum = forAll {a = Bool} {prop = Bool} arbitrary (MkFn (\_ =>
  applyDifferenceMap emptyVexel emptySubstrate == emptySubstrate))

||| Divergence map on vacuum produces vacuum 0-cochain
prop_divergenceVacuum : Property
prop_divergenceVacuum = forAll {a = Bool} {prop = Bool} arbitrary (MkFn (\_ =>
  applyDivergenceMap emptySubstrate == emptyVexel))

||| Laplacian Δ = Divergence ∘ Difference on vacuum produces empty vexel
prop_laplacianVacuum : Property
prop_laplacianVacuum = forAll {a = Bool} {prop = Bool} arbitrary (MkFn (\_ =>
  multisetLaplacian emptyVexel emptySubstrate == emptyVexel))

||| Discrete flux integral of empty substrate is 0
prop_fluxIntegralVacuum : Property
prop_fluxIntegralVacuum = forAll {a = Bool} {prop = Bool} arbitrary (MkFn (\_ =>
  discreteFluxIntegral emptySubstrate == 0))

||| Substrate integral of vacuum state is 0
prop_substrateIntegralVacuum : Property
prop_substrateIntegralVacuum = forAll {a = Bool} {prop = Bool} arbitrary (MkFn (\_ =>
  substrateIntegral emptySubstrate emptyVexel == 0))

||| Leibniz integral lag recurrence: L(S k) = (S k) + L(k)
prop_leibnizLagRecurrence : Property
prop_leibnizLagRecurrence = forAll {a = Nat} {prop = Bool} arbitrary (MkFn (\k =>
  leibnizIntegralLag (S k) == (S k) + leibnizIntegralLag k))

||| Hodge decomposition of vacuum state yields harmonic empty components
prop_hodgeDecomposeVacuum : Property
prop_hodgeDecomposeVacuum = forAll {a = Bool} {prop = Bool} arbitrary (MkFn (\_ =>
  let (MkHodge harm grad curl) = hodgeDecompose emptyVexel emptySubstrate
  in harm == emptyVexel && grad == emptyVexel && curl == emptyVexel))

--------------------------------------------------------------------------------
-- 4. TEST RUNNER
--------------------------------------------------------------------------------

partial
runSuite : IO ()
runSuite = do
  putStrLn ""
  putStrLn "--------------------------------------------------------"
  putStrLn "-- idris2-Substrate: Discrete Substrate Test Suite    --"
  putStrLn "--------------------------------------------------------"
  putStrLn ""

  let r1  = quickCheck prop_substrateMergeCommutative
  putStrLn $ "prop_substrateMergeCommutative: " ++ r1.msg

  let r2  = quickCheck prop_substrateMergeIdentity
  putStrLn $ "prop_substrateMergeIdentity: " ++ r2.msg

  let r3  = quickCheck prop_vacuumSubstrateLag
  putStrLn $ "prop_vacuumSubstrateLag: " ++ r3.msg

  let r4  = quickCheck prop_singleEdgeLag
  putStrLn $ "prop_singleEdgeLag: " ++ r4.msg

  let r5  = quickCheck prop_superposeCommutative
  putStrLn $ "prop_superposeCommutative: " ++ r5.msg

  let r6  = quickCheck prop_superposeIdentity
  putStrLn $ "prop_superposeIdentity: " ++ r6.msg

  let r7  = quickCheck prop_restrictToPixelEmpty
  putStrLn $ "prop_restrictToPixelEmpty: " ++ r7.msg

  let r8  = quickCheck prop_synchronisedVacuum
  putStrLn $ "prop_synchronisedVacuum: " ++ r8.msg

  let r9  = quickCheck prop_differenceVacuum
  putStrLn $ "prop_differenceVacuum: " ++ r9.msg

  let r10 = quickCheck prop_divergenceVacuum
  putStrLn $ "prop_divergenceVacuum: " ++ r10.msg

  let r11 = quickCheck prop_laplacianVacuum
  putStrLn $ "prop_laplacianVacuum: " ++ r11.msg

  let r12 = quickCheck prop_fluxIntegralVacuum
  putStrLn $ "prop_fluxIntegralVacuum: " ++ r12.msg

  let r13 = quickCheck prop_substrateIntegralVacuum
  putStrLn $ "prop_substrateIntegralVacuum: " ++ r13.msg

  let r14 = quickCheck prop_leibnizLagRecurrence
  putStrLn $ "prop_leibnizLagRecurrence: " ++ r14.msg

  let r15 = quickCheck prop_hodgeDecomposeVacuum
  putStrLn $ "prop_hodgeDecomposeVacuum: " ++ r15.msg

  let results = [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15]
  let failures = filter (\r => isJust r.pass && fromMaybe True r.pass == False) results
  if null failures
    then putStrLn "\nAll 15 Substrate tests passed."
    else idris_crash "❌ FAILURE: One or more Substrate properties failed verification."

partial
main : IO ()
main = runSuite
