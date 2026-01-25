module Test.Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class.Console (log)
import Purelint.Fix (applyAllFixes)
import Purelint.Tidy (formatExpr)
import Purelint.Rules.FmapId (fmapIdRule)
import Purelint.Rules.MapFusion (mapFusionRule)
import Purelint.Rules.UseTraverse (useTraverseRule)
import Purelint.Rules.ConcatMap (concatMapRule)
import Purelint.Rules.NotEqual (notEqualRule)
import Purelint.Rules.UseGuard (useGuardRule)
import Purelint.Rules.EtaReduce (etaReduceRule)
import Purelint.Rules.EtaReduceDecl (etaReduceDeclRule)
import Purelint.Rules.RedundantBind (redundantBindRule)
import Purelint.Rules.LetToWhere (letToWhereRule)
import Purelint.Rules.RedundantIf (redundantIfRule)
import Purelint.Rules.FunctorLaw (functorLawRule)
import Purelint.Rules.UseVoid (useVoidRule)
import Purelint.Rules.UseJoin (useJoinRule)
import Purelint.Rules.UseAny (useAnyRule)
import Purelint.Rules.UseAll (useAllRule)
import Purelint.Rules.BooleanSimplify (booleanSimplifyRule)
import Purelint.Rules.CollapseLambdas (collapseLambdasRule)
import Purelint.Rules.UseConst (useConstRule)
import Purelint.Rules.UseNull (useNullRule)
import Purelint.Rules.UseFromMaybe (useFromMaybeRule)
import Purelint.Rules.UseIsJust (useIsJustRule)
import Purelint.Rules.UseIsNothing (useIsNothingRule)
import Purelint.Rules.UseWhen (useWhenRule)
import Purelint.Rules.UseUnless (useUnlessRule)
import Purelint.Rules.RedundantFlip (redundantFlipRule)
import Purelint.Rules.UseNotElem (useNotElemRule)
import Purelint.Rules.UseMinMax (useMinMaxRule)
import Purelint.Rules.MonoidIdentity (monoidIdentityRule)
import Purelint.Rules.UseFold (useFoldRule)
import Purelint.Rules.UseSequence (useSequenceRule)
import Purelint.Rules.UseTraverseSequence (useTraverseSequenceRule)
import Purelint.Rules.RedundantNot (redundantNotRule)
import Purelint.Rules.UseFstSnd (useFstSndRule)
import Purelint.Rules.UseMapMaybe (useMapMaybeRule)
import Purelint.Rules.UseGuardMaybe (useGuardMaybeRule)
import Purelint.Rules.UseMaybeMap (useMaybeMapRule)
import Purelint.Rules.UseApplicative (useApplicativeRule)
import Purelint.Rules.UseBindFlip (useBindFlipRule)
import Purelint.Rules.UseFor (useForRule)
import Purelint.Rules.RedundantGuard (redundantGuardRule)
import Purelint.Rules.UseComparing (useComparingRule)
import Purelint.Rules.UseOn (useOnRule)
import Purelint.Rules.UseFindMap (useFindMapRule)
import Purelint.Rules.UseLastReverse (useLastReverseRule)
import Purelint.Rules.RedundantReverse (redundantReverseRule)
import Purelint.Rules.UseBreak (useBreakRule)
import Purelint.Rules.UseSpan (useSpanRule)
import Purelint.Rules.UseMinimumSort (useMinimumSortRule)
import Purelint.Rules.UseBimap (useBimapRule)
import Purelint.Rules.UseEitherMap (useEitherMapRule)
import Purelint.Rules.EvaluateFst (evaluateFstRule)
import Purelint.Rules.EvaluateConst (evaluateConstRule)
import Purelint.Rules.UseOr (useOrRule)
import Purelint.Rules.UseAnd (useAndRule)
import Purelint.Rules.UseFoldMapId (useFoldMapIdRule)
import Purelint.Rules.WhenNot (whenNotRule)
import Purelint.Rules.UnlessNot (unlessNotRule)
import Purelint.Rules.UseZip (useZipRule)
import Purelint.Rules.UseReplicate (useReplicateRule)
import Purelint.Rules.UseUncurry (useUncurryRule)
import Purelint.Rules.RedundantNegate (redundantNegateRule)
import Purelint.Rules.UseFromJust (useFromJustRule)
import Purelint.Rules.UseHead (useHeadRule)
import Purelint.Rules.RedundantId (redundantIdRule)
import Purelint.Rules.NothingBind (nothingBindRule)
import Purelint.Rules.UseElemIndex (useElemIndexRule)
import Purelint.Rule (Rule)
import Purelint.Runner (runRules, getImportsFromSource, getModuleNames)
import Purelint.Imports (hasValue, isPreludeModule)
import Purelint.Types (LintResult(..), LintWarning(..), RuleId(..), SourceCode(..), Suggestion(..))

-- | Simple test framework
type TestResult = { passed :: Boolean, name :: String, message :: String }

runTest :: String -> Boolean -> String -> TestResult
runTest name passed message = { passed, name, message }

-- | Assert that running rules produces exactly n warnings
assertWarningCount :: String -> Array Rule -> String -> Int -> TestResult
assertWarningCount name rules source expected =
  case runRules rules (SourceCode source) of
    Left err -> runTest name false ("Parse error: " <> err)
    Right (LintResult result) ->
      let actual = Array.length result.warnings
      in if actual == expected
         then runTest name true "OK"
         else runTest name false ("Expected " <> show expected <> " warnings, got " <> show actual)

-- | Assert that the first warning has a specific rule ID
assertRuleId :: String -> Array Rule -> String -> String -> TestResult
assertRuleId name rules source expectedRuleId =
  case runRules rules (SourceCode source) of
    Left err -> runTest name false ("Parse error: " <> err)
    Right (LintResult result) ->
      case Array.head result.warnings of
        Nothing -> runTest name false "No warnings produced"
        Just (LintWarning w) ->
          let (RuleId rid) = w.ruleId
          in if rid == expectedRuleId
             then runTest name true "OK"
             else runTest name false ("Expected rule " <> expectedRuleId <> ", got " <> rid)

-- | Assert that the first warning has a suggestion with expected replacement
assertSuggestion :: String -> Array Rule -> String -> String -> TestResult
assertSuggestion name rules source expectedReplacement =
  case runRules rules (SourceCode source) of
    Left err -> runTest name false ("Parse error: " <> err)
    Right (LintResult result) ->
      case Array.head result.warnings of
        Nothing -> runTest name false "No warnings produced"
        Just (LintWarning w) ->
          case w.suggestion of
            Nothing -> runTest name false "No suggestion in warning"
            Just (Suggestion s) ->
              let actual = unwrap s.replacement
              in if actual == expectedReplacement
                 then runTest name true "OK"
                 else runTest name false ("Expected suggestion '" <> expectedReplacement <> "', got '" <> actual <> "'")

-- | Assert that applying fixes produces expected output
assertFix :: String -> Array Rule -> String -> String -> TestResult
assertFix name rules source expectedFixed =
  case runRules rules (SourceCode source) of
    Left err -> runTest name false ("Parse error: " <> err)
    Right result ->
      let fixed = applyAllFixes source result
      in if fixed == expectedFixed
         then runTest name true "OK"
         else runTest name false ("Expected:\n" <> expectedFixed <> "\nGot:\n" <> fixed)

-- | Run all tests and report results
main :: Effect Unit
main = do
  log "Running purelint tests...\n"
  
  let results = fmapIdTests 
             <> mapFusionTests
             <> useTraverseTests
             <> concatMapTests
             <> notEqualTests
             <> useGuardTests
             <> etaReduceTests
             <> etaReduceDeclTests
             <> redundantBindTests
             <> letToWhereTests
             <> redundantIfTests
             <> functorLawTests
             <> useVoidTests
             <> useJoinTests
             <> useAnyTests
             <> useAllTests
             <> booleanSimplifyTests
             <> collapseLambdasTests
             <> useConstTests
             <> useNullTests
             <> useFromMaybeTests
             <> useIsJustTests
             <> useIsNothingTests
             <> useWhenTests
             <> useUnlessTests
             <> redundantFlipTests
             <> useNotElemTests
             <> useMinMaxTests
             <> monoidIdentityTests
             <> useFoldTests
             <> useSequenceTests
             <> useTraverseSequenceTests
             <> redundantNotTests
             <> useFstSndTests
             <> useMapMaybeTests
             <> useApplicativeTests
             <> useBindFlipTests
             <> useForTests
             <> redundantGuardTests
             <> useComparingTests
             <> useOnTests
             <> useFindMapTests
             <> useLastReverseTests
             <> redundantReverseTests
             <> useMinimumSortTests
             <> useBimapTests
             <> evaluateFstTests
             <> evaluateConstTests
             <> useOrTests
             <> useAndTests
             <> useFoldMapIdTests
             <> whenNotTests
             <> unlessNotTests
             <> useZipTests
             <> redundantNegateTests
             <> redundantIdTests
             <> nothingBindTests
             <> goldenTests
  
  let passed = Array.length $ Array.filter _.passed results
  let total = Array.length results
  
  -- Report failures
  let failures = Array.filter (not <<< _.passed) results
  when (Array.length failures > 0) do
    log "FAILURES:"
    for_ failures (\r -> log ("  ✗ " <> r.name <> ": " <> r.message))
    log ""
  
  -- Report summary
  log $ "Tests: " <> show passed <> "/" <> show total <> " passed"
  
  when (passed < total) do
    log "\nSome tests failed!"

  -- Run purs-tidy formatting tests
  log "\nRunning purs-tidy formatting tests..."
  tidyResults <- runTidyTests
  let tidyPassed = Array.length $ Array.filter _.passed tidyResults
  let tidyTotal = Array.length tidyResults
  
  let tidyFailures = Array.filter (not <<< _.passed) tidyResults
  when (Array.length tidyFailures > 0) do
    log "TIDY FAILURES:"
    for_ tidyFailures (\r -> log ("  ✗ " <> r.name <> ": " <> r.message))
    log ""
  
  log $ "Tidy tests: " <> show tidyPassed <> "/" <> show tidyTotal <> " passed"

-- | Helper: wrap code in a module with Prelude import
withPrelude :: String -> String
withPrelude code = "module Test where\nimport Prelude\nimport Data.Maybe\nimport Data.Foldable\nimport Data.Tuple\nimport Data.Monoid\nimport Data.Traversable\nimport Data.Array\nimport Control.Alternative\nimport Data.Ord\nimport Data.Either\nimport Data.Bifunctor\nimport Data.List\n" <> code

-- | Helper: wrap code in module without imports
withoutImports :: String -> String
withoutImports code = "module Test where\n" <> code

-- ============================================================================
-- FmapId Tests
-- ============================================================================

fmapIdTests :: Array TestResult
fmapIdTests =
  -- Positive cases: should detect
  [ assertWarningCount "FmapId: map identity x" [fmapIdRule] 
      (withPrelude "x = map identity [1]") 1
  , assertRuleId "FmapId: correct rule id" [fmapIdRule]
      (withPrelude "x = map identity [1]") "FmapId"
  , assertSuggestion "FmapId: map identity suggestion" [fmapIdRule]
      (withPrelude "x = map identity [1]") "[1]"
  , assertWarningCount "FmapId: fmap identity x" [fmapIdRule]
      (withPrelude "x = fmap identity [1]") 1
  , assertWarningCount "FmapId: identity <$> x" [fmapIdRule]
      (withPrelude "x = identity <$> [1]") 1
  , assertWarningCount "FmapId: x <#> identity" [fmapIdRule]
      (withPrelude "x = [1] <#> identity") 1
  
  -- Negative cases: should NOT detect
  , assertWarningCount "FmapId: no fire on map show" [fmapIdRule]
      (withPrelude "x = map show [1]") 0
  , assertWarningCount "FmapId: no fire without import" [fmapIdRule]
      (withoutImports "x = map identity [1]") 0
  
  -- Auto-fix test
  , assertFix "FmapId: fix map identity" [fmapIdRule]
      (withPrelude "x = map identity [1]")
      (withPrelude "x = [1]")
  ]

-- ============================================================================
-- MapFusion Tests
-- ============================================================================

mapFusionTests :: Array TestResult
mapFusionTests =
  -- Positive cases
  [ assertWarningCount "MapFusion: map f (map g x)" [mapFusionRule]
      (withPrelude "x = map f (map g [1])") 1
  , assertRuleId "MapFusion: correct rule id" [mapFusionRule]
      (withPrelude "x = map f (map g [1])") "MapFusion"
  , assertWarningCount "MapFusion: f <$> (g <$> x)" [mapFusionRule]
      (withPrelude "x = f <$> (g <$> [1])") 1
  , assertWarningCount "MapFusion: x <#> g <#> f" [mapFusionRule]
      (withPrelude "x = [1] <#> g <#> f") 1
  
  -- Negative cases
  , assertWarningCount "MapFusion: no fire on single map" [mapFusionRule]
      (withPrelude "x = map f [1]") 0
  , assertWarningCount "MapFusion: no fire without import" [mapFusionRule]
      (withoutImports "x = map f (map g [1])") 0
  ]

-- ============================================================================
-- UseTraverse Tests
-- ============================================================================

useTraverseTests :: Array TestResult
useTraverseTests =
  -- Positive cases
  [ assertWarningCount "UseTraverse: sequenceA (map f x)" [useTraverseRule]
      (withPrelude "x = sequenceA (map f [1])") 1
  , assertRuleId "UseTraverse: correct rule id" [useTraverseRule]
      (withPrelude "x = sequenceA (map f [1])") "UseTraverse"
  , assertWarningCount "UseTraverse: sequenceA (f <$> x)" [useTraverseRule]
      (withPrelude "x = sequenceA (f <$> [1])") 1
  , assertWarningCount "UseTraverse: sequence (map f x)" [useTraverseRule]
      (withPrelude "x = sequence (map f [1])") 1
  
  -- Negative cases
  , assertWarningCount "UseTraverse: no fire on sequenceA alone" [useTraverseRule]
      (withPrelude "x = sequenceA [1]") 0
  , assertWarningCount "UseTraverse: no fire without import" [useTraverseRule]
      (withoutImports "x = sequenceA (map f [1])") 0
  ]

-- ============================================================================
-- ConcatMap Tests
-- ============================================================================

concatMapTests :: Array TestResult
concatMapTests =
  -- Positive cases
  [ assertWarningCount "ConcatMap: concat (map f x)" [concatMapRule]
      (withPrelude "x = concat (map f [1])") 1
  , assertRuleId "ConcatMap: correct rule id" [concatMapRule]
      (withPrelude "x = concat (map f [1])") "ConcatMap"
  , assertWarningCount "ConcatMap: join (map f x)" [concatMapRule]
      (withPrelude "x = join (map f [1])") 1
  
  -- Negative cases
  , assertWarningCount "ConcatMap: no fire on concat alone" [concatMapRule]
      (withPrelude "x = concat [[1]]") 0
  , assertWarningCount "ConcatMap: no fire without import" [concatMapRule]
      (withoutImports "x = concat (map f [1])") 0
  ]

-- ============================================================================
-- NotEqual Tests
-- ============================================================================

notEqualTests :: Array TestResult
notEqualTests =
  -- Positive cases
  [ assertWarningCount "NotEqual: not (a == b)" [notEqualRule]
      (withPrelude "x = not (1 == 2)") 1
  , assertRuleId "NotEqual: correct rule id" [notEqualRule]
      (withPrelude "x = not (1 == 2)") "NotEqual"
  , assertWarningCount "NotEqual: not (a /= b)" [notEqualRule]
      (withPrelude "x = not (1 /= 2)") 1
  
  -- Negative cases
  , assertWarningCount "NotEqual: no fire on not (a > b)" [notEqualRule]
      (withPrelude "x = not (1 > 2)") 0
  , assertWarningCount "NotEqual: no fire without import" [notEqualRule]
      (withoutImports "x = not (1 == 2)") 0
  ]

-- ============================================================================
-- UseGuard Tests
-- ============================================================================

useGuardTests :: Array TestResult
useGuardTests =
  -- Positive cases
  [ assertWarningCount "UseGuard: if cond then x else pure unit" [useGuardRule]
      (withPrelude "x = if true then log \"hi\" else pure unit") 1
  , assertRuleId "UseGuard: correct rule id" [useGuardRule]
      (withPrelude "x = if true then log \"hi\" else pure unit") "UseGuard"
  , assertWarningCount "UseGuard: if cond then pure unit else x" [useGuardRule]
      (withPrelude "x = if true then pure unit else log \"hi\"") 1
  
  -- Negative cases
  , assertWarningCount "UseGuard: no fire when neither is pure unit" [useGuardRule]
      (withPrelude "x = if true then log \"a\" else log \"b\"") 0
  , assertWarningCount "UseGuard: no fire without import" [useGuardRule]
      (withoutImports "x = if true then log \"hi\" else pure unit") 0
  ]

-- ============================================================================
-- EtaReduce Tests
-- ============================================================================

etaReduceTests :: Array TestResult
etaReduceTests =
  -- Positive cases
  [ assertWarningCount "EtaReduce: \\x -> f x" [etaReduceRule]
      (withPrelude "x = \\a -> f a") 1
  , assertRuleId "EtaReduce: correct rule id" [etaReduceRule]
      (withPrelude "x = \\a -> f a") "EtaReduce"
  , assertSuggestion "EtaReduce: suggestion" [etaReduceRule]
      (withPrelude "x = \\a -> f a") "f"
  
  -- Negative cases
  , assertWarningCount "EtaReduce: no fire on \\x -> f x x" [etaReduceRule]
      (withPrelude "x = \\a -> f a a") 0
  , assertWarningCount "EtaReduce: no fire on \\x -> x" [etaReduceRule]
      (withPrelude "x = \\a -> a") 0
  ]

-- ============================================================================
-- EtaReduceDecl Tests
-- ============================================================================

etaReduceDeclTests :: Array TestResult
etaReduceDeclTests =
  -- Positive cases
  [ assertWarningCount "EtaReduceDecl: foo x = bar x" [etaReduceDeclRule]
      (withPrelude "foo x = bar x") 1
  , assertRuleId "EtaReduceDecl: correct rule id" [etaReduceDeclRule]
      (withPrelude "foo x = bar x") "EtaReduceDecl"
  , assertWarningCount "EtaReduceDecl: foo a b = bar a b" [etaReduceDeclRule]
      (withPrelude "foo a b = bar a b") 1
  
  -- Negative cases
  , assertWarningCount "EtaReduceDecl: no fire on foo x = bar x x" [etaReduceDeclRule]
      (withPrelude "foo x = bar x x") 0
  , assertWarningCount "EtaReduceDecl: no fire on foo x = x" [etaReduceDeclRule]
      (withPrelude "foo x = x") 0
  ]

-- ============================================================================
-- RedundantBind Tests
-- ============================================================================

redundantBindTests :: Array TestResult
redundantBindTests =
  -- Positive cases
  [ assertWarningCount "RedundantBind: x >>= pure" [redundantBindRule]
      (withPrelude "x = [1] >>= pure") 1
  , assertRuleId "RedundantBind: correct rule id" [redundantBindRule]
      (withPrelude "x = [1] >>= pure") "RedundantBind"
  , assertSuggestion "RedundantBind: x >>= pure suggestion" [redundantBindRule]
      (withPrelude "x = [1] >>= pure") "[1]"
  , assertWarningCount "RedundantBind: pure x >>= f" [redundantBindRule]
      (withPrelude "x = pure 1 >>= show") 1
  , assertSuggestion "RedundantBind: pure x >>= f suggestion" [redundantBindRule]
      (withPrelude "x = pure 1 >>= show") "show 1"
  
  -- Negative cases
  , assertWarningCount "RedundantBind: no fire on x >>= f" [redundantBindRule]
      (withPrelude "x = [1] >>= show") 0
  , assertWarningCount "RedundantBind: no fire without import" [redundantBindRule]
      (withoutImports "x = [1] >>= pure") 0
  ]

-- ============================================================================
-- LetToWhere Tests
-- ============================================================================

letToWhereTests :: Array TestResult
letToWhereTests =
  -- Positive cases
  [ assertWarningCount "LetToWhere: let x = 1 in x + 1" [letToWhereRule]
      (withPrelude "x = let y = 1 in y + 1") 1
  , assertRuleId "LetToWhere: correct rule id" [letToWhereRule]
      (withPrelude "x = let y = 1 in y + 1") "LetToWhere"
  
  -- Multiple bindings
  , assertWarningCount "LetToWhere: multiple bindings" [letToWhereRule]
      (withPrelude "x = let a = 1\n        b = 2 in a + b") 1
  
  -- Suggestion preserves spaces
  , assertSuggestion "LetToWhere: preserves spaces in simple case" [letToWhereRule]
      (withPrelude "x = let y = 1 in y + 1") "y + 1\n  where\n  y = 1"
  , assertSuggestion "LetToWhere: preserves spaces in expressions" [letToWhereRule]
      (withPrelude "f x = let a = x + 1 in a * 2") "a * 2\n  where\n  a = x + 1"
  , assertSuggestion "LetToWhere: preserves spaces with multiple bindings" [letToWhereRule]
      (withPrelude "f x = let a = x + 1\n          b = x * 2 in a + b") "a + b\n  where\n  a = x + 1\n  b = x * 2"
  ]

-- ============================================================================
-- RedundantIf Tests
-- ============================================================================

redundantIfTests :: Array TestResult
redundantIfTests =
  -- Positive cases
  [ assertWarningCount "RedundantIf: if a then true else false" [redundantIfRule]
      (withPrelude "x = if foo then true else false") 1
  , assertRuleId "RedundantIf: correct rule id" [redundantIfRule]
      (withPrelude "x = if foo then true else false") "RedundantIf"
  , assertSuggestion "RedundantIf: suggests condition" [redundantIfRule]
      (withPrelude "x = if foo then true else false") "foo"
  , assertWarningCount "RedundantIf: if a then false else true" [redundantIfRule]
      (withPrelude "x = if foo then false else true") 1
  , assertSuggestion "RedundantIf: suggests not condition" [redundantIfRule]
      (withPrelude "x = if foo then false else true") "not foo"
  
  -- Negative cases
  , assertWarningCount "RedundantIf: no fire on normal if" [redundantIfRule]
      (withPrelude "x = if foo then 1 else 2") 0
  , assertWarningCount "RedundantIf: no fire on if a then true else x" [redundantIfRule]
      (withPrelude "x = if foo then true else bar") 0
  ]

-- ============================================================================
-- FunctorLaw Tests
-- ============================================================================

functorLawTests :: Array TestResult
functorLawTests =
  -- Positive cases
  [ assertWarningCount "FunctorLaw: map f (map g x)" [functorLawRule]
      (withPrelude "x = map f (map g [1])") 1
  , assertRuleId "FunctorLaw: correct rule id" [functorLawRule]
      (withPrelude "x = map f (map g [1])") "FunctorLaw"
  , assertWarningCount "FunctorLaw: f <$> (g <$> x)" [functorLawRule]
      (withPrelude "x = f <$> (g <$> [1])") 1
  
  -- Negative cases
  , assertWarningCount "FunctorLaw: no fire on single map" [functorLawRule]
      (withPrelude "x = map f [1]") 0
  , assertWarningCount "FunctorLaw: no fire without import" [functorLawRule]
      (withoutImports "x = map f (map g [1])") 0
  ]

-- ============================================================================
-- UseVoid Tests
-- ============================================================================

useVoidTests :: Array TestResult
useVoidTests =
  -- Positive cases
  [ assertWarningCount "UseVoid: a *> pure unit" [useVoidRule]
      (withPrelude "x = foo *> pure unit") 1
  , assertRuleId "UseVoid: correct rule id" [useVoidRule]
      (withPrelude "x = foo *> pure unit") "UseVoid"
  , assertSuggestion "UseVoid: suggests void" [useVoidRule]
      (withPrelude "x = foo *> pure unit") "void foo"
  
  -- Negative cases
  , assertWarningCount "UseVoid: no fire on a *> pure x" [useVoidRule]
      (withPrelude "x = foo *> pure 1") 0
  , assertWarningCount "UseVoid: no fire without import" [useVoidRule]
      (withoutImports "x = foo *> pure unit") 0
  ]

-- ============================================================================
-- UseJoin Tests
-- ============================================================================

useJoinTests :: Array TestResult
useJoinTests =
  -- Positive cases
  [ assertWarningCount "UseJoin: x >>= identity" [useJoinRule]
      (withPrelude "x = foo >>= identity") 1
  , assertRuleId "UseJoin: correct rule id" [useJoinRule]
      (withPrelude "x = foo >>= identity") "UseJoin"
  , assertSuggestion "UseJoin: suggests join" [useJoinRule]
      (withPrelude "x = foo >>= identity") "join foo"
  
  -- Negative cases
  , assertWarningCount "UseJoin: no fire on x >>= f" [useJoinRule]
      (withPrelude "x = foo >>= show") 0
  , assertWarningCount "UseJoin: no fire without import" [useJoinRule]
      (withoutImports "x = foo >>= identity") 0
  ]

-- ============================================================================
-- UseAny Tests
-- ============================================================================

useAnyTests :: Array TestResult
useAnyTests =
  -- Positive cases
  [ assertWarningCount "UseAny: or (map f x)" [useAnyRule]
      (withPrelude "x = or (map f [1])") 1
  , assertRuleId "UseAny: correct rule id" [useAnyRule]
      (withPrelude "x = or (map f [1])") "UseAny"
  , assertSuggestion "UseAny: suggests any" [useAnyRule]
      (withPrelude "x = or (map f [1])") "any f [1]"
  
  -- Negative cases
  , assertWarningCount "UseAny: no fire on or alone" [useAnyRule]
      (withPrelude "x = or [true]") 0
  , assertWarningCount "UseAny: no fire without import" [useAnyRule]
      (withoutImports "x = or (map f [1])") 0
  ]

-- ============================================================================
-- UseAll Tests
-- ============================================================================

useAllTests :: Array TestResult
useAllTests =
  -- Positive cases
  [ assertWarningCount "UseAll: and (map f x)" [useAllRule]
      (withPrelude "x = and (map f [1])") 1
  , assertRuleId "UseAll: correct rule id" [useAllRule]
      (withPrelude "x = and (map f [1])") "UseAll"
  , assertSuggestion "UseAll: suggests all" [useAllRule]
      (withPrelude "x = and (map f [1])") "all f [1]"
  
  -- Negative cases
  , assertWarningCount "UseAll: no fire on and alone" [useAllRule]
      (withPrelude "x = and [true]") 0
  , assertWarningCount "UseAll: no fire without import" [useAllRule]
      (withoutImports "x = and (map f [1])") 0
  ]

-- ============================================================================
-- BooleanSimplify Tests
-- ============================================================================

booleanSimplifyTests :: Array TestResult
booleanSimplifyTests =
  -- Positive cases
  [ assertWarningCount "BooleanSimplify: x == true" [booleanSimplifyRule]
      (withPrelude "x = foo == true") 1
  , assertRuleId "BooleanSimplify: correct rule id" [booleanSimplifyRule]
      (withPrelude "x = foo == true") "BooleanSimplify"
  , assertSuggestion "BooleanSimplify: x == true -> x" [booleanSimplifyRule]
      (withPrelude "x = foo == true") "foo"
  , assertWarningCount "BooleanSimplify: x == false" [booleanSimplifyRule]
      (withPrelude "x = foo == false") 1
  , assertSuggestion "BooleanSimplify: x == false -> not x" [booleanSimplifyRule]
      (withPrelude "x = foo == false") "not foo"
  , assertWarningCount "BooleanSimplify: true == x" [booleanSimplifyRule]
      (withPrelude "x = true == foo") 1
  , assertWarningCount "BooleanSimplify: false == x" [booleanSimplifyRule]
      (withPrelude "x = false == foo") 1
  
  -- Negative cases
  , assertWarningCount "BooleanSimplify: no fire on x == y" [booleanSimplifyRule]
      (withPrelude "x = foo == bar") 0
  , assertWarningCount "BooleanSimplify: no fire without import" [booleanSimplifyRule]
      (withoutImports "x = foo == true") 0
  ]

-- ============================================================================
-- CollapseLambdas Tests
-- ============================================================================

collapseLambdasTests :: Array TestResult
collapseLambdasTests =
  -- Positive cases
  [ assertWarningCount "CollapseLambdas: \\x -> \\y -> body" [collapseLambdasRule]
      (withPrelude "x = \\a -> \\b -> a + b") 1
  , assertRuleId "CollapseLambdas: correct rule id" [collapseLambdasRule]
      (withPrelude "x = \\a -> \\b -> a + b") "CollapseLambdas"
  , assertSuggestion "CollapseLambdas: suggests collapsed" [collapseLambdasRule]
      (withPrelude "x = \\a -> \\b -> a + b") "\\a b -> a + b"
  
  -- Negative cases
  , assertWarningCount "CollapseLambdas: no fire on single lambda" [collapseLambdasRule]
      (withPrelude "x = \\a -> a + 1") 0
  ]

-- ============================================================================
-- UseConst Tests
-- ============================================================================

useConstTests :: Array TestResult
useConstTests =
  -- Positive cases
  [ assertWarningCount "UseConst: \\x -> y (unused)" [useConstRule]
      (withPrelude "x = \\a -> 42") 1
  , assertRuleId "UseConst: correct rule id" [useConstRule]
      (withPrelude "x = \\a -> 42") "UseConst"
  , assertSuggestion "UseConst: suggests const" [useConstRule]
      (withPrelude "x = \\a -> 42") "const 42"
  
  -- Negative cases
  , assertWarningCount "UseConst: no fire when param used" [useConstRule]
      (withPrelude "x = \\a -> a + 1") 0
  , assertWarningCount "UseConst: no fire without import" [useConstRule]
      (withoutImports "x = \\a -> 42") 0
  ]

-- ============================================================================
-- UseNull Tests
-- ============================================================================

useNullTests :: Array TestResult
useNullTests =
  -- Positive cases
  [ assertWarningCount "UseNull: length x == 0" [useNullRule]
      (withPrelude "x = length [1] == 0") 1
  , assertRuleId "UseNull: correct rule id" [useNullRule]
      (withPrelude "x = length [1] == 0") "UseNull"
  , assertSuggestion "UseNull: suggests null" [useNullRule]
      (withPrelude "x = length [1] == 0") "null [1]"
  , assertWarningCount "UseNull: 0 == length x" [useNullRule]
      (withPrelude "x = 0 == length [1]") 1
  
  -- Negative cases
  , assertWarningCount "UseNull: no fire on length x == 1" [useNullRule]
      (withPrelude "x = length [1] == 1") 0
  , assertWarningCount "UseNull: no fire without import" [useNullRule]
      (withoutImports "x = length [1] == 0") 0
  ]

-- ============================================================================
-- UseFromMaybe Tests
-- ============================================================================

useFromMaybeTests :: Array TestResult
useFromMaybeTests =
  [ assertWarningCount "UseFromMaybe: maybe x identity" [useFromMaybeRule]
      (withPrelude "x = maybe 0 identity") 1
  , assertRuleId "UseFromMaybe: correct rule id" [useFromMaybeRule]
      (withPrelude "x = maybe 0 identity") "UseFromMaybe"
  , assertSuggestion "UseFromMaybe: suggests fromMaybe" [useFromMaybeRule]
      (withPrelude "x = maybe 0 identity") "fromMaybe 0"
  
  -- Negative cases
  , assertWarningCount "UseFromMaybe: no fire on maybe x f" [useFromMaybeRule]
      (withPrelude "x = maybe 0 show") 0
  ]

-- ============================================================================
-- UseIsJust Tests
-- ============================================================================

useIsJustTests :: Array TestResult
useIsJustTests =
  [ assertWarningCount "UseIsJust: maybe false (const true)" [useIsJustRule]
      (withPrelude "x = maybe false (const true)") 1
  , assertRuleId "UseIsJust: correct rule id" [useIsJustRule]
      (withPrelude "x = maybe false (const true)") "UseIsJust"
  , assertSuggestion "UseIsJust: suggests isJust" [useIsJustRule]
      (withPrelude "x = maybe false (const true)") "isJust"
  
  -- Negative cases
  , assertWarningCount "UseIsJust: no fire on maybe true (const false)" [useIsJustRule]
      (withPrelude "x = maybe true (const false)") 0
  ]

-- ============================================================================
-- UseIsNothing Tests
-- ============================================================================

useIsNothingTests :: Array TestResult
useIsNothingTests =
  [ assertWarningCount "UseIsNothing: maybe true (const false)" [useIsNothingRule]
      (withPrelude "x = maybe true (const false)") 1
  , assertRuleId "UseIsNothing: correct rule id" [useIsNothingRule]
      (withPrelude "x = maybe true (const false)") "UseIsNothing"
  , assertSuggestion "UseIsNothing: suggests isNothing" [useIsNothingRule]
      (withPrelude "x = maybe true (const false)") "isNothing"
  
  -- Negative cases
  , assertWarningCount "UseIsNothing: no fire on maybe false (const true)" [useIsNothingRule]
      (withPrelude "x = maybe false (const true)") 0
  ]

-- ============================================================================
-- UseWhen Tests
-- ============================================================================

useWhenTests :: Array TestResult
useWhenTests =
  [ assertWarningCount "UseWhen: if x then y else pure unit" [useWhenRule]
      (withPrelude "x = if true then log \"hi\" else pure unit") 1
  , assertRuleId "UseWhen: correct rule id" [useWhenRule]
      (withPrelude "x = if true then log \"hi\" else pure unit") "UseWhen"
  
  -- Negative cases
  , assertWarningCount "UseWhen: no fire on if x then pure unit else y" [useWhenRule]
      (withPrelude "x = if true then pure unit else log \"hi\"") 0
  ]

-- ============================================================================
-- UseUnless Tests
-- ============================================================================

useUnlessTests :: Array TestResult
useUnlessTests =
  [ assertWarningCount "UseUnless: if x then pure unit else y" [useUnlessRule]
      (withPrelude "x = if true then pure unit else log \"hi\"") 1
  , assertRuleId "UseUnless: correct rule id" [useUnlessRule]
      (withPrelude "x = if true then pure unit else log \"hi\"") "UseUnless"
  
  -- Negative cases
  , assertWarningCount "UseUnless: no fire on if x then y else pure unit" [useUnlessRule]
      (withPrelude "x = if true then log \"hi\" else pure unit") 0
  ]

-- ============================================================================
-- RedundantFlip Tests
-- ============================================================================

redundantFlipTests :: Array TestResult
redundantFlipTests =
  [ assertWarningCount "RedundantFlip: flip (flip f)" [redundantFlipRule]
      (withPrelude "x = flip (flip add)") 1
  , assertRuleId "RedundantFlip: correct rule id" [redundantFlipRule]
      (withPrelude "x = flip (flip add)") "RedundantFlip"
  , assertSuggestion "RedundantFlip: suggests f" [redundantFlipRule]
      (withPrelude "x = flip (flip add)") "add"
  
  -- Negative cases
  , assertWarningCount "RedundantFlip: no fire on flip f" [redundantFlipRule]
      (withPrelude "x = flip add") 0
  ]

-- ============================================================================
-- UseNotElem Tests
-- ============================================================================

useNotElemTests :: Array TestResult
useNotElemTests =
  [ assertWarningCount "UseNotElem: not (elem x y)" [useNotElemRule]
      (withPrelude "x = not (elem 1 [1,2,3])") 1
  , assertRuleId "UseNotElem: correct rule id" [useNotElemRule]
      (withPrelude "x = not (elem 1 [1,2,3])") "UseNotElem"
  , assertSuggestion "UseNotElem: suggests notElem" [useNotElemRule]
      (withPrelude "x = not (elem 1 [1,2,3])") "notElem 1 [1,2,3]"
  
  -- Negative cases
  , assertWarningCount "UseNotElem: no fire on not (foo x)" [useNotElemRule]
      (withPrelude "x = not (foo 1)") 0
  ]

-- ============================================================================
-- UseMinMax Tests
-- ============================================================================

useMinMaxTests :: Array TestResult
useMinMaxTests =
  [ assertWarningCount "UseMinMax: if a > b then a else b" [useMinMaxRule]
      (withPrelude "x = if a > b then a else b") 1
  , assertRuleId "UseMinMax: correct rule id" [useMinMaxRule]
      (withPrelude "x = if a > b then a else b") "UseMinMax"
  , assertSuggestion "UseMinMax: suggests max" [useMinMaxRule]
      (withPrelude "x = if a > b then a else b") "max a b"
  , assertWarningCount "UseMinMax: if a < b then a else b -> min" [useMinMaxRule]
      (withPrelude "x = if a < b then a else b") 1
  , assertSuggestion "UseMinMax: suggests min" [useMinMaxRule]
      (withPrelude "x = if a < b then a else b") "min a b"
  
  -- Negative cases
  , assertWarningCount "UseMinMax: no fire on unrelated if" [useMinMaxRule]
      (withPrelude "x = if a > b then c else d") 0
  ]

-- ============================================================================
-- MonoidIdentity Tests
-- ============================================================================

monoidIdentityTests :: Array TestResult
monoidIdentityTests =
  [ assertWarningCount "MonoidIdentity: mempty <> x" [monoidIdentityRule]
      (withPrelude "x = mempty <> [1,2,3]") 1
  , assertRuleId "MonoidIdentity: correct rule id" [monoidIdentityRule]
      (withPrelude "x = mempty <> [1,2,3]") "MonoidIdentity"
  , assertSuggestion "MonoidIdentity: suggests x" [monoidIdentityRule]
      (withPrelude "x = mempty <> [1,2,3]") "[1,2,3]"
  , assertWarningCount "MonoidIdentity: x <> mempty" [monoidIdentityRule]
      (withPrelude "x = [1,2,3] <> mempty") 1
  
  -- Negative cases
  , assertWarningCount "MonoidIdentity: no fire on x <> y" [monoidIdentityRule]
      (withPrelude "x = [1] <> [2]") 0
  ]

-- ============================================================================
-- UseFold Tests
-- ============================================================================

useFoldTests :: Array TestResult
useFoldTests =
  [ assertWarningCount "UseFold: foldMap identity" [useFoldRule]
      (withPrelude "x = foldMap identity") 1
  , assertRuleId "UseFold: correct rule id" [useFoldRule]
      (withPrelude "x = foldMap identity") "UseFold"
  , assertSuggestion "UseFold: suggests fold" [useFoldRule]
      (withPrelude "x = foldMap identity") "fold"
  
  -- Negative cases
  , assertWarningCount "UseFold: no fire on foldMap f" [useFoldRule]
      (withPrelude "x = foldMap show") 0
  ]

-- ============================================================================
-- UseSequence Tests
-- ============================================================================

useSequenceTests :: Array TestResult
useSequenceTests =
  [ assertWarningCount "UseSequence: traverse identity" [useSequenceRule]
      (withPrelude "x = traverse identity") 1
  , assertRuleId "UseSequence: correct rule id" [useSequenceRule]
      (withPrelude "x = traverse identity") "UseSequence"
  , assertSuggestion "UseSequence: suggests sequence" [useSequenceRule]
      (withPrelude "x = traverse identity") "sequence"
  
  -- Negative cases
  , assertWarningCount "UseSequence: no fire on traverse f" [useSequenceRule]
      (withPrelude "x = traverse show") 0
  ]

-- ============================================================================
-- UseTraverseSequence Tests
-- ============================================================================

useTraverseSequenceTests :: Array TestResult
useTraverseSequenceTests =
  [ assertWarningCount "UseTraverseSequence: sequence (map f x)" [useTraverseSequenceRule]
      (withPrelude "x = sequence (map show [1,2,3])") 1
  , assertRuleId "UseTraverseSequence: correct rule id" [useTraverseSequenceRule]
      (withPrelude "x = sequence (map show [1,2,3])") "UseTraverseSequence"
  , assertSuggestion "UseTraverseSequence: suggests traverse" [useTraverseSequenceRule]
      (withPrelude "x = sequence (map show [1,2,3])") "traverse show [1,2,3]"
  
  -- Negative cases
  , assertWarningCount "UseTraverseSequence: no fire on sequence xs" [useTraverseSequenceRule]
      (withPrelude "x = sequence [Just 1]") 0
  ]

-- ============================================================================
-- RedundantNot Tests
-- ============================================================================

redundantNotTests :: Array TestResult
redundantNotTests =
  [ assertWarningCount "RedundantNot: not (not x)" [redundantNotRule]
      (withPrelude "x = not (not true)") 1
  , assertRuleId "RedundantNot: correct rule id" [redundantNotRule]
      (withPrelude "x = not (not true)") "RedundantNot"
  , assertSuggestion "RedundantNot: suggests x" [redundantNotRule]
      (withPrelude "x = not (not foo)") "foo"
  
  -- Negative cases
  , assertWarningCount "RedundantNot: no fire on not x" [redundantNotRule]
      (withPrelude "x = not true") 0
  ]

-- ============================================================================
-- UseFstSnd Tests
-- ============================================================================

useFstSndTests :: Array TestResult
useFstSndTests =
  [ assertWarningCount "UseFstSnd: \\(Tuple x y) -> x" [useFstSndRule]
      (withPrelude "x = \\(Tuple a b) -> a") 1
  , assertRuleId "UseFstSnd: correct rule id" [useFstSndRule]
      (withPrelude "x = \\(Tuple a b) -> a") "UseFstSnd"
  , assertSuggestion "UseFstSnd: suggests fst" [useFstSndRule]
      (withPrelude "x = \\(Tuple a b) -> a") "fst"
  , assertWarningCount "UseFstSnd: \\(Tuple x y) -> y" [useFstSndRule]
      (withPrelude "x = \\(Tuple a b) -> b") 1
  , assertSuggestion "UseFstSnd: suggests snd" [useFstSndRule]
      (withPrelude "x = \\(Tuple a b) -> b") "snd"
  
  -- Negative cases
  , assertWarningCount "UseFstSnd: no fire when using both" [useFstSndRule]
      (withPrelude "x = \\(Tuple a b) -> a + b") 0
  ]

-- ============================================================================
-- UseMapMaybe Tests
-- ============================================================================

useMapMaybeTests :: Array TestResult
useMapMaybeTests =
  [ assertWarningCount "UseMapMaybe: catMaybes (map f x)" [useMapMaybeRule]
      (withPrelude "x = catMaybes (map f [1,2,3])") 1
  , assertRuleId "UseMapMaybe: correct rule id" [useMapMaybeRule]
      (withPrelude "x = catMaybes (map f [1,2,3])") "UseMapMaybe"
  , assertSuggestion "UseMapMaybe: suggests mapMaybe" [useMapMaybeRule]
      (withPrelude "x = catMaybes (map f [1,2,3])") "mapMaybe f [1,2,3]"
  ]

-- ============================================================================
-- UseApplicative Tests
-- ============================================================================

useApplicativeTests :: Array TestResult
useApplicativeTests =
  [ assertWarningCount "UseApplicative: pure f <*> x" [useApplicativeRule]
      (withPrelude "x = pure f <*> y") 1
  , assertRuleId "UseApplicative: correct rule id" [useApplicativeRule]
      (withPrelude "x = pure f <*> y") "UseApplicative"
  , assertSuggestion "UseApplicative: suggests f <$> x" [useApplicativeRule]
      (withPrelude "x = pure f <*> y") "f <$> y"
  ]

-- ============================================================================
-- UseBindFlip Tests
-- ============================================================================

useBindFlipTests :: Array TestResult
useBindFlipTests =
  [ assertWarningCount "UseBindFlip: join (map f x)" [useBindFlipRule]
      (withPrelude "x = join (map f y)") 1
  , assertRuleId "UseBindFlip: correct rule id" [useBindFlipRule]
      (withPrelude "x = join (map f y)") "UseBindFlip"
  , assertSuggestion "UseBindFlip: suggests x >>= f" [useBindFlipRule]
      (withPrelude "x = join (map f y)") "y >>= f"
  ]

-- ============================================================================
-- UseFor Tests
-- ============================================================================

useForTests :: Array TestResult
useForTests =
  [ assertWarningCount "UseFor: flip traverse" [useForRule]
      (withPrelude "x = flip traverse") 1
  , assertRuleId "UseFor: correct rule id" [useForRule]
      (withPrelude "x = flip traverse") "UseFor"
  , assertSuggestion "UseFor: suggests for" [useForRule]
      (withPrelude "x = flip traverse") "for"
  , assertWarningCount "UseFor: flip traverse_" [useForRule]
      (withPrelude "x = flip traverse_") 1
  , assertSuggestion "UseFor: suggests for_" [useForRule]
      (withPrelude "x = flip traverse_") "for_"
  ]

-- ============================================================================
-- RedundantGuard Tests
-- ============================================================================

redundantGuardTests :: Array TestResult
redundantGuardTests =
  [ assertWarningCount "RedundantGuard: guard true" [redundantGuardRule]
      (withPrelude "x = guard true") 1
  , assertRuleId "RedundantGuard: correct rule id" [redundantGuardRule]
      (withPrelude "x = guard true") "RedundantGuard"
  , assertSuggestion "RedundantGuard: suggests pure unit" [redundantGuardRule]
      (withPrelude "x = guard true") "pure unit"
  , assertWarningCount "RedundantGuard: guard false" [redundantGuardRule]
      (withPrelude "x = guard false") 1
  ]

-- ============================================================================
-- UseComparing Tests
-- ============================================================================

useComparingTests :: Array TestResult
useComparingTests =
  [ assertWarningCount "UseComparing: \\x y -> compare (f x) (f y)" [useComparingRule]
      (withPrelude "x = \\a b -> compare (f a) (f b)") 1
  , assertRuleId "UseComparing: correct rule id" [useComparingRule]
      (withPrelude "x = \\a b -> compare (f a) (f b)") "UseComparing"
  , assertSuggestion "UseComparing: suggests comparing" [useComparingRule]
      (withPrelude "x = \\a b -> compare (f a) (f b)") "comparing f"
  ]

-- ============================================================================
-- UseOn Tests
-- ============================================================================

useOnTests :: Array TestResult
useOnTests =
  [ assertWarningCount "UseOn: \\x y -> f (g x) (g y)" [useOnRule]
      (withPrelude "x = \\a b -> f (g a) (g b)") 1
  , assertRuleId "UseOn: correct rule id" [useOnRule]
      (withPrelude "x = \\a b -> f (g a) (g b)") "UseOn"
  , assertSuggestion "UseOn: suggests on f g" [useOnRule]
      (withPrelude "x = \\a b -> f (g a) (g b)") "on f g"
  ]

-- ============================================================================
-- UseFindMap Tests
-- ============================================================================

useFindMapTests :: Array TestResult
useFindMapTests =
  [ assertWarningCount "UseFindMap: head (mapMaybe f x)" [useFindMapRule]
      (withPrelude "x = head (mapMaybe f [1,2,3])") 1
  , assertRuleId "UseFindMap: correct rule id" [useFindMapRule]
      (withPrelude "x = head (mapMaybe f [1,2,3])") "UseFindMap"
  , assertSuggestion "UseFindMap: suggests findMap" [useFindMapRule]
      (withPrelude "x = head (mapMaybe f [1,2,3])") "findMap f [1,2,3]"
  ]

-- ============================================================================
-- UseLastReverse Tests
-- ============================================================================

useLastReverseTests :: Array TestResult
useLastReverseTests =
  [ assertWarningCount "UseLastReverse: head (reverse x)" [useLastReverseRule]
      (withPrelude "x = head (reverse [1,2,3])") 1
  , assertRuleId "UseLastReverse: correct rule id" [useLastReverseRule]
      (withPrelude "x = head (reverse [1,2,3])") "UseLastReverse"
  , assertSuggestion "UseLastReverse: suggests last" [useLastReverseRule]
      (withPrelude "x = head (reverse [1,2,3])") "last [1,2,3]"
  ]

-- ============================================================================
-- RedundantReverse Tests
-- ============================================================================

redundantReverseTests :: Array TestResult
redundantReverseTests =
  [ assertWarningCount "RedundantReverse: reverse (reverse x)" [redundantReverseRule]
      (withPrelude "x = reverse (reverse [1,2,3])") 1
  , assertRuleId "RedundantReverse: correct rule id" [redundantReverseRule]
      (withPrelude "x = reverse (reverse [1,2,3])") "RedundantReverse"
  , assertSuggestion "RedundantReverse: suggests x" [redundantReverseRule]
      (withPrelude "x = reverse (reverse foo)") "foo"
  ]

-- ============================================================================
-- UseMinimumSort Tests
-- ============================================================================

useMinimumSortTests :: Array TestResult
useMinimumSortTests =
  [ assertWarningCount "UseMinimumSort: head (sort x)" [useMinimumSortRule]
      (withPrelude "x = head (sort [1,2,3])") 1
  , assertRuleId "UseMinimumSort: correct rule id" [useMinimumSortRule]
      (withPrelude "x = head (sort [1,2,3])") "UseMinimumSort"
  , assertSuggestion "UseMinimumSort: suggests minimum" [useMinimumSortRule]
      (withPrelude "x = head (sort [1,2,3])") "minimum [1,2,3]"
  ]

-- ============================================================================
-- UseBimap Tests
-- ============================================================================

useBimapTests :: Array TestResult
useBimapTests =
  [ assertWarningCount "UseBimap: bimap id g" [useBimapRule]
      (withPrelude "x = bimap identity show") 1
  , assertRuleId "UseBimap: correct rule id" [useBimapRule]
      (withPrelude "x = bimap identity show") "UseBimap"
  , assertSuggestion "UseBimap: suggests second" [useBimapRule]
      (withPrelude "x = bimap identity show") "second show"
  ]

-- ============================================================================
-- EvaluateFst Tests
-- ============================================================================

evaluateFstTests :: Array TestResult
evaluateFstTests =
  [ assertWarningCount "EvaluateFst: fst (Tuple x y)" [evaluateFstRule]
      (withPrelude "x = fst (Tuple 1 2)") 1
  , assertRuleId "EvaluateFst: correct rule id" [evaluateFstRule]
      (withPrelude "x = fst (Tuple 1 2)") "EvaluateFst"
  , assertSuggestion "EvaluateFst: suggests x" [evaluateFstRule]
      (withPrelude "x = fst (Tuple 1 2)") "1"
  , assertWarningCount "EvaluateFst: snd (Tuple x y)" [evaluateFstRule]
      (withPrelude "x = snd (Tuple 1 2)") 1
  , assertSuggestion "EvaluateFst: suggests y" [evaluateFstRule]
      (withPrelude "x = snd (Tuple 1 2)") "2"
  ]

-- ============================================================================
-- EvaluateConst Tests
-- ============================================================================

evaluateConstTests :: Array TestResult
evaluateConstTests =
  [ assertWarningCount "EvaluateConst: const x y" [evaluateConstRule]
      (withPrelude "x = const 42 foo") 1
  , assertRuleId "EvaluateConst: correct rule id" [evaluateConstRule]
      (withPrelude "x = const 42 foo") "EvaluateConst"
  , assertSuggestion "EvaluateConst: suggests x" [evaluateConstRule]
      (withPrelude "x = const 42 foo") "42"
  ]

-- ============================================================================
-- UseOr Tests
-- ============================================================================

useOrTests :: Array TestResult
useOrTests =
  [ assertWarningCount "UseOr: any identity" [useOrRule]
      (withPrelude "x = any identity [true, false]") 1
  , assertRuleId "UseOr: correct rule id" [useOrRule]
      (withPrelude "x = any identity [true, false]") "UseOr"
  , assertSuggestion "UseOr: suggests or" [useOrRule]
      (withPrelude "x = any identity [true, false]") "or [true, false]"
  ]

-- ============================================================================
-- UseAnd Tests
-- ============================================================================

useAndTests :: Array TestResult
useAndTests =
  [ assertWarningCount "UseAnd: all identity" [useAndRule]
      (withPrelude "x = all identity [true, false]") 1
  , assertRuleId "UseAnd: correct rule id" [useAndRule]
      (withPrelude "x = all identity [true, false]") "UseAnd"
  , assertSuggestion "UseAnd: suggests and" [useAndRule]
      (withPrelude "x = all identity [true, false]") "and [true, false]"
  ]

-- ============================================================================
-- UseFoldMapId Tests
-- ============================================================================

useFoldMapIdTests :: Array TestResult
useFoldMapIdTests =
  [ assertWarningCount "UseFoldMapId: foldMap identity" [useFoldMapIdRule]
      (withPrelude "x = foldMap identity") 1
  , assertRuleId "UseFoldMapId: correct rule id" [useFoldMapIdRule]
      (withPrelude "x = foldMap identity") "UseFoldMapId"
  , assertSuggestion "UseFoldMapId: suggests fold" [useFoldMapIdRule]
      (withPrelude "x = foldMap identity") "fold"
  ]

-- ============================================================================
-- WhenNot Tests
-- ============================================================================

whenNotTests :: Array TestResult
whenNotTests =
  [ assertWarningCount "WhenNot: when (not x)" [whenNotRule]
      (withPrelude "x = when (not cond) action") 1
  , assertRuleId "WhenNot: correct rule id" [whenNotRule]
      (withPrelude "x = when (not cond) action") "WhenNot"
  , assertSuggestion "WhenNot: suggests unless" [whenNotRule]
      (withPrelude "x = when (not cond) action") "unless cond action"
  ]

-- ============================================================================
-- UnlessNot Tests
-- ============================================================================

unlessNotTests :: Array TestResult
unlessNotTests =
  [ assertWarningCount "UnlessNot: unless (not x)" [unlessNotRule]
      (withPrelude "x = unless (not cond) action") 1
  , assertRuleId "UnlessNot: correct rule id" [unlessNotRule]
      (withPrelude "x = unless (not cond) action") "UnlessNot"
  , assertSuggestion "UnlessNot: suggests when" [unlessNotRule]
      (withPrelude "x = unless (not cond) action") "when cond action"
  ]

-- ============================================================================
-- UseZip Tests
-- ============================================================================

useZipTests :: Array TestResult
useZipTests =
  [ assertWarningCount "UseZip: zipWith Tuple" [useZipRule]
      (withPrelude "x = zipWith Tuple [1] [2]") 1
  , assertRuleId "UseZip: correct rule id" [useZipRule]
      (withPrelude "x = zipWith Tuple [1] [2]") "UseZip"
  , assertSuggestion "UseZip: suggests zip" [useZipRule]
      (withPrelude "x = zipWith Tuple [1] [2]") "zip [1] [2]"
  ]

-- ============================================================================
-- RedundantNegate Tests
-- ============================================================================

redundantNegateTests :: Array TestResult
redundantNegateTests =
  [ assertWarningCount "RedundantNegate: negate (negate x)" [redundantNegateRule]
      (withPrelude "x = negate (negate 5)") 1
  , assertRuleId "RedundantNegate: correct rule id" [redundantNegateRule]
      (withPrelude "x = negate (negate 5)") "RedundantNegate"
  , assertSuggestion "RedundantNegate: suggests x" [redundantNegateRule]
      (withPrelude "x = negate (negate foo)") "foo"
  ]

-- ============================================================================
-- RedundantId Tests
-- ============================================================================

redundantIdTests :: Array TestResult
redundantIdTests =
  [ assertWarningCount "RedundantId: identity x" [redundantIdRule]
      (withPrelude "x = identity 42") 1
  , assertRuleId "RedundantId: correct rule id" [redundantIdRule]
      (withPrelude "x = identity 42") "RedundantId"
  , assertSuggestion "RedundantId: suggests x" [redundantIdRule]
      (withPrelude "x = identity foo") "foo"
  ]

-- ============================================================================
-- NothingBind Tests
-- ============================================================================

nothingBindTests :: Array TestResult
nothingBindTests =
  [ assertWarningCount "NothingBind: Nothing >>= f" [nothingBindRule]
      (withPrelude "x = Nothing >>= show") 1
  , assertRuleId "NothingBind: correct rule id" [nothingBindRule]
      (withPrelude "x = Nothing >>= show") "NothingBind"
  , assertSuggestion "NothingBind: suggests Nothing" [nothingBindRule]
      (withPrelude "x = Nothing >>= show") "Nothing"
  ]

-- ============================================================================
-- Golden Tests: LintExamples.purs
-- ============================================================================

-- | All rules for golden testing
allRulesForGolden :: Array Rule
allRulesForGolden =
  [ fmapIdRule
  , mapFusionRule
  , useTraverseRule
  , concatMapRule
  , notEqualRule
  , useGuardRule
  , etaReduceRule
  , etaReduceDeclRule
  , redundantBindRule
  , letToWhereRule
  , redundantIfRule
  , functorLawRule
  , useVoidRule
  , useJoinRule
  , useAnyRule
  , useAllRule
  , booleanSimplifyRule
  , collapseLambdasRule
  , useConstRule
  , useNullRule
  , useFromMaybeRule
  , useIsJustRule
  , useIsNothingRule
  , useWhenRule
  , useUnlessRule
  , redundantFlipRule
  , useNotElemRule
  , useMinMaxRule
  , monoidIdentityRule
  , useFoldRule
  , useSequenceRule
  , useTraverseSequenceRule
  , redundantNotRule
  , useFstSndRule
  , useMapMaybeRule
  , useGuardMaybeRule
  , useMaybeMapRule
  , useApplicativeRule
  , useBindFlipRule
  , useForRule
  , redundantGuardRule
  , useComparingRule
  , useOnRule
  , useFindMapRule
  ]

-- | Assert that a specific rule fires at least once when running all rules
assertRuleFires :: String -> String -> String -> TestResult
assertRuleFires name source ruleId =
  case runRules allRulesForGolden (SourceCode source) of
    Left err -> runTest name false ("Parse error: " <> err)
    Right (LintResult result) ->
      let matches = Array.filter (\(LintWarning w) -> unwrap w.ruleId == ruleId) result.warnings
      in if Array.length matches > 0
         then runTest name true "OK"
         else runTest name false ("Rule " <> ruleId <> " did not fire")

-- | The embedded LintExamples source
lintExamplesSource :: String
lintExamplesSource = """module Test.LintExamples where

import Prelude

import Control.Alternative
import Control.Monad
import Data.Array
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Traversable
import Data.Tuple

-- FmapId
fmapId1 = map identity [ 1, 2, 3 ]

-- MapFusion
mapFusion1 = map show (map (_ + 1) [ 1, 2, 3 ])

-- UseTraverse
useTraverse1 f xs = sequence (map f xs)

-- ConcatMap
concatMap1 f xs = join (map f xs)

-- NotEqual
notEqual1 x y = not (x == y)

-- UseGuard
useGuard1 cond action = if cond then action else pure unit

-- EtaReduce
etaReduce1 = \x -> show x

-- EtaReduceDecl
etaReduceDecl1 x = show x

-- RedundantBind
redundantBind1 x f = pure x >>= f

-- LetToWhere
letToWhere1 y z = let x = y in z

-- RedundantIf
redundantIf1 a = if a then true else false

-- UseVoid
useVoid1 a = a *> pure unit

-- UseJoin
useJoin1 x = x >>= identity

-- UseAny
useAny1 f xs = or (map f xs)

-- UseAll
useAll1 f xs = and (map f xs)

-- BooleanSimplify
booleanSimplify1 x = x == true

-- CollapseLambdas
collapseLambdas1 = \x -> \y -> x + y

-- UseConst
useConst1 = \x -> 42

-- UseNull
useNull1 xs = length xs == 0

-- UseFromMaybe
useFromMaybe1 def = maybe def identity

-- UseIsJust
useIsJust1 = maybe false (const true)

-- UseIsNothing
useIsNothing1 = maybe true (const false)

-- UseWhen
useWhen1 cond action = if cond then action else pure unit

-- UseUnless
useUnless1 cond action = if cond then pure unit else action

-- RedundantFlip
redundantFlip1 f = flip (flip f)

-- UseNotElem
useNotElem1 x xs = not (elem x xs)

-- UseMinMax
useMax1 a b = if a > b then a else b

-- MonoidIdentity
monoidIdentity1 x = mempty <> x

-- UseFold
useFold1 = foldMap identity

-- UseSequence
useSequence1 = traverse identity

-- UseTraverseSequence
useTraverseSequence1 f xs = sequence (map f xs)

-- RedundantNot
redundantNot1 x = not (not x)

-- UseFstSnd
useFst1 = \(Tuple x y) -> x

-- UseMapMaybe
useMapMaybe1 = catMaybes (map f [1,2,3])

-- UseApplicative
useApplicative1 = pure f <*> x

-- UseBindFlip
useBindFlip1 = join (map f x)

-- UseFor
useFor1 = flip traverse

-- RedundantGuard
redundantGuard1 = guard true

-- UseComparing
useComparing1 = \a b -> compare (f a) (f b)

-- UseOn
useOn1 = \a b -> f (g a) (g b)

-- UseFindMap
useFindMap1 = head (mapMaybe f [1,2,3])
"""

goldenTests :: Array TestResult
goldenTests =
  [ assertRuleFires "Golden: FmapId fires" lintExamplesSource "FmapId"
  , assertRuleFires "Golden: MapFusion fires" lintExamplesSource "MapFusion"
  , assertRuleFires "Golden: UseTraverse fires" lintExamplesSource "UseTraverse"
  , assertRuleFires "Golden: ConcatMap fires" lintExamplesSource "ConcatMap"
  , assertRuleFires "Golden: NotEqual fires" lintExamplesSource "NotEqual"
  , assertRuleFires "Golden: UseGuard fires" lintExamplesSource "UseGuard"
  , assertRuleFires "Golden: EtaReduce fires" lintExamplesSource "EtaReduce"
  , assertRuleFires "Golden: EtaReduceDecl fires" lintExamplesSource "EtaReduceDecl"
  , assertRuleFires "Golden: RedundantBind fires" lintExamplesSource "RedundantBind"
  , assertRuleFires "Golden: LetToWhere fires" lintExamplesSource "LetToWhere"
  , assertRuleFires "Golden: RedundantIf fires" lintExamplesSource "RedundantIf"
  , assertRuleFires "Golden: UseVoid fires" lintExamplesSource "UseVoid"
  , assertRuleFires "Golden: UseJoin fires" lintExamplesSource "UseJoin"
  , assertRuleFires "Golden: UseAny fires" lintExamplesSource "UseAny"
  , assertRuleFires "Golden: UseAll fires" lintExamplesSource "UseAll"
  , assertRuleFires "Golden: BooleanSimplify fires" lintExamplesSource "BooleanSimplify"
  , assertRuleFires "Golden: CollapseLambdas fires" lintExamplesSource "CollapseLambdas"
  , assertRuleFires "Golden: UseConst fires" lintExamplesSource "UseConst"
  , assertRuleFires "Golden: UseNull fires" lintExamplesSource "UseNull"
  , assertRuleFires "Golden: UseFromMaybe fires" lintExamplesSource "UseFromMaybe"
  , assertRuleFires "Golden: UseIsJust fires" lintExamplesSource "UseIsJust"
  , assertRuleFires "Golden: UseIsNothing fires" lintExamplesSource "UseIsNothing"
  , assertRuleFires "Golden: UseWhen fires" lintExamplesSource "UseWhen"
  , assertRuleFires "Golden: UseUnless fires" lintExamplesSource "UseUnless"
  , assertRuleFires "Golden: RedundantFlip fires" lintExamplesSource "RedundantFlip"
  , assertRuleFires "Golden: UseNotElem fires" lintExamplesSource "UseNotElem"
  , assertRuleFires "Golden: UseMinMax fires" lintExamplesSource "UseMinMax"
  , assertRuleFires "Golden: MonoidIdentity fires" lintExamplesSource "MonoidIdentity"
  , assertRuleFires "Golden: UseFold fires" lintExamplesSource "UseFold"
  , assertRuleFires "Golden: UseSequence fires" lintExamplesSource "UseSequence"
  , assertRuleFires "Golden: UseTraverseSequence fires" lintExamplesSource "UseTraverseSequence"
  , assertRuleFires "Golden: RedundantNot fires" lintExamplesSource "RedundantNot"
  , assertRuleFires "Golden: UseFstSnd fires" lintExamplesSource "UseFstSnd"
  , assertRuleFires "Golden: UseMapMaybe fires" lintExamplesSource "UseMapMaybe"
  , assertRuleFires "Golden: UseApplicative fires" lintExamplesSource "UseApplicative"
  , assertRuleFires "Golden: UseBindFlip fires" lintExamplesSource "UseBindFlip"
  , assertRuleFires "Golden: UseFor fires" lintExamplesSource "UseFor"
  , assertRuleFires "Golden: RedundantGuard fires" lintExamplesSource "RedundantGuard"
  , assertRuleFires "Golden: UseComparing fires" lintExamplesSource "UseComparing"
  , assertRuleFires "Golden: UseOn fires" lintExamplesSource "UseOn"
  , assertRuleFires "Golden: UseFindMap fires" lintExamplesSource "UseFindMap"
  ]

-- ============================================================================
-- Purs-Tidy Formatting Tests
-- ============================================================================

-- | Test that a suggestion is already purs-tidy formatted
assertTidyFormatted :: String -> String -> Effect TestResult
assertTidyFormatted name suggestion = do
  formatted <- formatExpr suggestion
  pure $ if formatted == suggestion
    then runTest name true "OK"
    else runTest name false ("Not tidy-formatted. Got: '" <> formatted <> "' Expected: '" <> suggestion <> "'")

-- | Suggestions that should be purs-tidy formatted
tidySuggestions :: Array { name :: String, suggestion :: String }
tidySuggestions =
  [ { name: "void foo", suggestion: "void foo" }
  , { name: "join foo", suggestion: "join foo" }
  , { name: "any f xs", suggestion: "any f xs" }
  , { name: "all f xs", suggestion: "all f xs" }
  , { name: "const 42", suggestion: "const 42" }
  , { name: "notElem x xs", suggestion: "notElem x xs" }
  , { name: "max a b", suggestion: "max a b" }
  , { name: "min a b", suggestion: "min a b" }
  , { name: "null xs", suggestion: "null xs" }
  , { name: "fromMaybe x", suggestion: "fromMaybe x" }
  , { name: "isJust", suggestion: "isJust" }
  , { name: "isNothing", suggestion: "isNothing" }
  , { name: "when cond action", suggestion: "when cond action" }
  , { name: "unless cond action", suggestion: "unless cond action" }
  , { name: "fst", suggestion: "fst" }
  , { name: "snd", suggestion: "snd" }
  , { name: "fold", suggestion: "fold" }
  , { name: "sequence", suggestion: "sequence" }
  , { name: "traverse f xs", suggestion: "traverse f xs" }
  , { name: "collapsed lambda", suggestion: "\\a b -> a + b" }
  , { name: "map fusion", suggestion: "map (f <<< g) x" }
  , { name: "fmap fusion", suggestion: "(f <<< g) <$> x" }
  , { name: "flipped map fusion", suggestion: "x <#> (g >>> f)" }
  , { name: "where clause", suggestion: "y + 1\n  where\n  y = 1" }
  ]

-- | Run all tidy formatting tests
runTidyTests :: Effect (Array TestResult)
runTidyTests = traverse (\t -> assertTidyFormatted ("Tidy: " <> t.name) t.suggestion) tidySuggestions
