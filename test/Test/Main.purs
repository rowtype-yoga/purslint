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
import Purslint.Fix (applyAllFixes)
import Purslint.Tidy (formatExpr, formatModule)
import Purslint.Rules.MapIdentity (mapIdentityRule)
import Purslint.Rules.MapFusion (mapFusionRule)
import Purslint.Rules.UseTraverse (useTraverseRule)
import Purslint.Rules.ConcatMap (concatMapRule)
import Purslint.Rules.NotEqual (notEqualRule)
import Purslint.Rules.UseGuard (useGuardRule)
import Purslint.Rules.EtaReduce (etaReduceRule)
import Purslint.Rules.EtaReduceDecl (etaReduceDeclRule)
import Purslint.Rules.RedundantBind (redundantBindRule)
import Purslint.Rules.LetToWhere (letToWhereRule)
import Purslint.Rules.LetToDo (letToDoRule)
import Purslint.Rules.RedundantIf (redundantIfRule)
import Purslint.Rules.FunctorLaw (functorLawRule)
import Purslint.Rules.UseVoid (useVoidRule)
import Purslint.Rules.UseJoin (useJoinRule)
import Purslint.Rules.UseAny (useAnyRule)
import Purslint.Rules.UseAll (useAllRule)
import Purslint.Rules.BooleanSimplify (booleanSimplifyRule)
import Purslint.Rules.CollapseLambdas (collapseLambdasRule)
import Purslint.Rules.UseConst (useConstRule)
import Purslint.Rules.UseDollar (useDollarRule)
import Purslint.Rules.UseNull (useNullRule)
import Purslint.Rules.UseFromMaybe (useFromMaybeRule)
import Purslint.Rules.UseIsJust (useIsJustRule)
import Purslint.Rules.UseIsNothing (useIsNothingRule)
import Purslint.Rules.UseWhen (useWhenRule)
import Purslint.Rules.UseUnless (useUnlessRule)
import Purslint.Rules.RedundantFlip (redundantFlipRule)
import Purslint.Rules.UseNotElem (useNotElemRule)
import Purslint.Rules.UseMinMax (useMinMaxRule)
import Purslint.Rules.MonoidIdentity (monoidIdentityRule)
import Purslint.Rules.UseFold (useFoldRule)
import Purslint.Rules.UseSequence (useSequenceRule)
import Purslint.Rules.UseTraverseSequence (useTraverseSequenceRule)
import Purslint.Rules.RedundantNot (redundantNotRule)
import Purslint.Rules.UseFstSnd (useFstSndRule)
import Purslint.Rules.UseMapMaybe (useMapMaybeRule)
import Purslint.Rules.UseGuardMaybe (useGuardMaybeRule)
import Purslint.Rules.UseMaybeMap (useMaybeMapRule)
import Purslint.Rules.UseApplicative (useApplicativeRule)
import Purslint.Rules.UseBindFlip (useBindFlipRule)
import Purslint.Rules.UseFor (useForRule)
import Purslint.Rules.UseUnwrap (useUnwrapRule)
import Purslint.Rules.RedundantGuard (redundantGuardRule)
import Purslint.Rules.UseComparing (useComparingRule)
import Purslint.Rules.UseOn (useOnRule)
import Purslint.Rules.UseFindMap (useFindMapRule)
import Purslint.Rules.UseLastReverse (useLastReverseRule)
import Purslint.Rules.RedundantReverse (redundantReverseRule)
import Purslint.Rules.UseBreak (useBreakRule)
import Purslint.Rules.UseSpan (useSpanRule)
import Purslint.Rules.UseMinimumSort (useMinimumSortRule)
import Purslint.Rules.UseBimap (useBimapRule)
import Purslint.Rules.UseEitherMap (useEitherMapRule)
import Purslint.Rules.EvaluateFst (evaluateFstRule)
import Purslint.Rules.EvaluateConst (evaluateConstRule)
import Purslint.Rules.UseOr (useOrRule)
import Purslint.Rules.UseAnd (useAndRule)
import Purslint.Rules.UseFoldMap (useFoldMapRule)
import Purslint.Rules.UseFoldMapId (useFoldMapIdRule)
import Purslint.Rules.UseApplyFlipped (useApplyFlippedRule)
import Purslint.Rules.WhenNot (whenNotRule)
import Purslint.Rules.UnlessNot (unlessNotRule)
import Purslint.Rules.UseZip (useZipRule)
import Purslint.Rules.UseReplicate (useReplicateRule)
import Purslint.Rules.UseUncurry (useUncurryRule)
import Purslint.Rules.RedundantNegate (redundantNegateRule)
import Purslint.Rules.UseFromJust (useFromJustRule)
import Purslint.Rules.UseHead (useHeadRule)
import Purslint.Rules.RedundantId (redundantIdRule)
import Purslint.Rules.NothingBind (nothingBindRule)
import Purslint.Rules.UseElemIndex (useElemIndexRule)
import Purslint.Rules.MonadLaw (monadLawRule)
import Purslint.Rules.EvaluateBool (evaluateBoolRule)
import Purslint.Rules.UseFoldBool (useFoldBoolRule)
import Purslint.Rules.AlternativeLaw (alternativeLawRule)
import Purslint.Rules.EvaluateEither (evaluateEitherRule)
import Purslint.Rules.UseCaseOf (useCaseOfRule)
import Purslint.Rules.FlattenCase (flattenCaseRule)
import Purslint.Rules.UsePatternGuards (usePatternGuardsRule)
import Purslint.Rules.RedundantParens (redundantParensRule)
import Purslint.Rule (Rule)
import Purslint.Runner (runRules, getImportsFromSource, getModuleNames)
import Purslint.Imports (hasValue, isPreludeModule)
import Purslint.Types (LintResult(..), LintWarning(..), RuleId(..), SourceCode(..), Suggestion(..))

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

-- | Assert that applying fixes produces expected output AND result is tidy-formatted
assertFixFormatted :: String -> Array Rule -> String -> String -> Effect TestResult
assertFixFormatted name rules source expectedFixed =
  case runRules rules (SourceCode source) of
    Left err -> pure $ runTest name false ("Parse error: " <> err)
    Right result -> do
      let fixed = applyAllFixes source result
      -- Check that the fix output is already tidy-formatted
      tidied <- formatModule fixed
      if tidied /= fixed
        then pure $ runTest name false ("Fix output not tidy-formatted.\nGot:\n" <> fixed <> "\nTidy expects:\n" <> tidied)
        else if fixed /= expectedFixed
          then pure $ runTest name false ("Expected:\n" <> expectedFixed <> "\nGot:\n" <> fixed)
          else pure $ runTest name true "OK"

-- | Run all tests and report results
main :: Effect Unit
main = do
  log "Running purslint tests...\n"
  
  let results = mapIdentityTests 
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
             <> useFoldMapTests
             <> useApplyFlippedTests
             <> useFoldMapIdTests
             <> whenNotTests
             <> unlessNotTests
             <> useZipTests
             <> useReplicateTests
             <> useUncurryTests
             <> useFromJustTests
             <> useHeadTests
             <> useElemIndexTests
             <> useBreakTests
             <> useSpanTests
             <> useDollarTests
             <> useUnwrapTests
             <> redundantNegateTests
             <> redundantIdTests
             <> nothingBindTests
             <> monadLawTests
             <> evaluateBoolTests
             <> useFoldBoolTests
             <> alternativeLawTests
             <> evaluateEitherTests
             <> useCaseOfTests
             <> flattenCaseTests
             <> usePatternGuardsTests
             <> redundantParensTests
  
  -- Run effectful golden tests (with purs-tidy check)
  goldenResults <- goldenTests
  let allResults = results <> goldenResults
  
  let passed = Array.length $ Array.filter _.passed allResults
  let total = Array.length allResults
  
  -- Report failures
  let failures = Array.filter (not <<< _.passed) allResults
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
withPrelude code = "module Test where\n\nimport Prelude\nimport Data.Maybe\nimport Data.Foldable\nimport Data.Tuple\nimport Data.Monoid\nimport Data.Traversable\nimport Data.Array\nimport Control.Alternative\nimport Data.Ord\nimport Data.Either\nimport Data.Bifunctor\nimport Data.List\n\n" <> code

-- | Helper: wrap code in module without imports
withoutImports :: String -> String
withoutImports code = "module Test where\n" <> code

-- ============================================================================
-- MapIdentity Tests
-- ============================================================================

mapIdentityTests :: Array TestResult
mapIdentityTests =
  -- Positive cases: should detect
  [ assertWarningCount "MapIdentity: map identity x" [mapIdentityRule] 
      (withPrelude "x = map identity [1]") 1
  , assertRuleId "MapIdentity: correct rule id" [mapIdentityRule]
      (withPrelude "x = map identity [1]") "MapIdentity"
  , assertSuggestion "MapIdentity: map identity suggestion" [mapIdentityRule]
      (withPrelude "x = map identity [1]") "[ 1 ]"
  , assertWarningCount "MapIdentity: identity <$> x" [mapIdentityRule]
      (withPrelude "x = identity <$> [1]") 1
  , assertWarningCount "MapIdentity: x <#> identity" [mapIdentityRule]
      (withPrelude "x = [1] <#> identity") 1
  
  -- Negative cases: should NOT detect
  , assertWarningCount "MapIdentity: no fire on map show" [mapIdentityRule]
      (withPrelude "x = map show [1]") 0
  , assertWarningCount "MapIdentity: no fire without import" [mapIdentityRule]
      (withoutImports "x = map identity [1]") 0
  
  -- Auto-fix test
  , assertFix "MapIdentity: fix map identity" [mapIdentityRule]
      (withPrelude "x = map identity [1]")
      (withPrelude "x = [ 1 ]")
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
      (withPrelude "x = [1] >>= pure") "[ 1 ]"
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
  
  -- Suggestion preserves spaces based on let column position
  , assertSuggestion "LetToWhere: preserves spaces in simple case" [letToWhereRule]
      (withPrelude "x = let y = 1 in y + 1") "y + 1\n    where\n    y = 1"
  , assertSuggestion "LetToWhere: preserves spaces in expressions" [letToWhereRule]
      (withPrelude "f x = let a = x + 1 in a * 2") "a * 2\n      where\n      a = x + 1"
  , assertSuggestion "LetToWhere: preserves spaces with multiple bindings" [letToWhereRule]
      (withPrelude "f x = let a = x + 1\n          b = x * 2 in a + b") "a + b\n      where\n      a = x + 1\n      b = x * 2"
  -- Nested let should preserve indentation context
  , assertSuggestion "LetToWhere: nested let preserves indentation" [letToWhereRule]
      (withPrelude "f x =\n    let y = 1 in y + 1") "y + 1\n    where\n    y = 1"
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
      (withPrelude "x = or (map f [1])") "any f [ 1 ]"
  
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
      (withPrelude "x = and (map f [1])") "all f [ 1 ]"
  
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
  -- Positive cases: only fires on wildcard binder \_
  [ assertWarningCount "UseConst: \\_ -> y" [useConstRule]
      (withPrelude "x = \\_ -> 42") 1
  , assertRuleId "UseConst: correct rule id" [useConstRule]
      (withPrelude "x = \\_ -> 42") "UseConst"
  , assertSuggestion "UseConst: suggests const" [useConstRule]
      (withPrelude "x = \\_ -> 42") "const 42"
  
  -- Negative cases
  , assertWarningCount "UseConst: no fire on named param (compiler warns)" [useConstRule]
      (withPrelude "x = \\a -> 42") 0
  , assertWarningCount "UseConst: no fire when param used" [useConstRule]
      (withPrelude "x = \\a -> a + 1") 0
  , assertWarningCount "UseConst: no fire without import" [useConstRule]
      (withoutImports "x = \\_ -> 42") 0
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
      (withPrelude "x = length [1] == 0") "null [ 1 ]"
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
      (withPrelude "x = not (elem 1 [1,2,3])") "notElem 1 [ 1, 2, 3 ]"
  
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
      (withPrelude "x = mempty <> [1,2,3]") "[ 1, 2, 3 ]"
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
      (withPrelude "x = sequence (map show [1,2,3])") "traverse show [ 1, 2, 3 ]"
  
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
      (withPrelude "x = catMaybes (map f [1,2,3])") "mapMaybe f [ 1, 2, 3 ]"
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
      (withPrelude "x = head (mapMaybe f [1,2,3])") "findMap f [ 1, 2, 3 ]"
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
      (withPrelude "x = head (reverse [1,2,3])") "last [ 1, 2, 3 ]"
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
      (withPrelude "x = head (sort [1,2,3])") "minimum [ 1, 2, 3 ]"
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
      (withPrelude "x = any identity [true, false]") "or [ true, false ]"
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
      (withPrelude "x = all identity [true, false]") "and [ true, false ]"
  ]

-- ============================================================================
-- UseFoldMap Tests
-- ============================================================================

useFoldMapTests :: Array TestResult
useFoldMapTests =
  [ assertWarningCount "UseFoldMap: case Nothing/Just with empty string" [useFoldMapRule]
      (withPrelude "x mx = case mx of\n  Nothing -> \"\"\n  Just s -> f s") 1
  , assertRuleId "UseFoldMap: correct rule id" [useFoldMapRule]
      (withPrelude "x mx = case mx of\n  Nothing -> \"\"\n  Just s -> f s") "UseFoldMap"
  , assertSuggestion "UseFoldMap: suggests foldMap f" [useFoldMapRule]
      (withPrelude "x mx = case mx of\n  Nothing -> \"\"\n  Just s -> f s") "foldMap f mx"
  , assertSuggestion "UseFoldMap: suggests lambda for complex body" [useFoldMapRule]
      (withPrelude "x mx = case mx of\n  Nothing -> \"\"\n  Just s -> s <> \"!\"") "foldMap (\\s -> s <> \"!\") mx"
  , assertWarningCount "UseFoldMap: reversed order Just/Nothing" [useFoldMapRule]
      (withPrelude "x mx = case mx of\n  Just s -> f s\n  Nothing -> \"\"") 1
  
  -- Complex scrutinee should be wrapped in parens
  , assertSuggestion "UseFoldMap: complex scrutinee gets parens" [useFoldMapRule]
      (withPrelude "x f mx = case f mx of\n  Nothing -> \"\"\n  Just s -> g s") "foldMap g (f mx)"
  
  -- Negative cases: should NOT detect
  , assertWarningCount "UseFoldMap: no fire on guarded Just branch" [useFoldMapRule]
      (withPrelude "x mx = case mx of\n  Just s | s > \"\" -> f s\n  _ -> \"\"") 0
  , assertWarningCount "UseFoldMap: no fire on where binding in Just branch" [useFoldMapRule]
      (withPrelude "x mx = case mx of\n  Just s -> g s where g = f\n  Nothing -> \"\"") 0
  
  -- Multiline body inside if-then-else should generate proper suggestion with correct indentation
  , assertSuggestion "UseFoldMap: multiline in if-then-else" [useFoldMapRule]
      (withPrelude "checkExpr args = case args of\n  [firstArg, secondArg] ->\n    if isLeft firstArg then\n      case getRightComposed secondArg of\n        Just f ->\n          let fText = show f\n          in\n            [ fText ]\n        Nothing -> []\n    else []") "foldMap\n        ( \\f ->\n            let fText = show f\n            in\n              [ fText ]\n        )\n        (getRightComposed secondArg)"
  ]

-- ============================================================================
-- UseApplyFlipped Tests
-- ============================================================================

useApplyFlippedTests :: Array TestResult
useApplyFlippedTests =
  [ assertWarningCount "UseApplyFlipped: f lambda arg" [useApplyFlippedRule]
      (withPrelude "x = foldMap (\\y -> y <> \"!\") xs") 1
  , assertRuleId "UseApplyFlipped: correct rule id" [useApplyFlippedRule]
      (withPrelude "x = foldMap (\\y -> y <> \"!\") xs") "UseApplyFlipped"
  , assertSuggestion "UseApplyFlipped: suggests # form" [useApplyFlippedRule]
      (withPrelude "x = foldMap (\\y -> y <> \"!\") xs") "xs # foldMap \\y -> y <> \"!\""
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
      (withPrelude "x = zipWith Tuple [1] [2]") "zip [ 1 ] [ 2 ]"
  ]

-- ============================================================================
-- UseReplicate Tests
-- ============================================================================

useReplicateTests :: Array TestResult
useReplicateTests =
  [ assertWarningCount "UseReplicate: take n (repeat x)" [useReplicateRule]
      (withPrelude "x = take 3 (repeat 1)") 1
  , assertRuleId "UseReplicate: correct rule id" [useReplicateRule]
      (withPrelude "x = take 3 (repeat 1)") "UseReplicate"
  , assertSuggestion "UseReplicate: suggests replicate" [useReplicateRule]
      (withPrelude "x = take 3 (repeat 1)") "replicate 3 1"
  , assertWarningCount "UseReplicate: no fire on take n xs" [useReplicateRule]
      (withPrelude "x = take 3 xs") 0
  , assertWarningCount "UseReplicate: no fire without import" [useReplicateRule]
      (withoutImports "x = take 3 (repeat 1)") 0
  ]

-- ============================================================================
-- UseUncurry Tests
-- ============================================================================

useUncurryTests :: Array TestResult
useUncurryTests =
  [ assertWarningCount "UseUncurry: f (fst p) (snd p)" [useUncurryRule]
      (withPrelude "x = f (fst p) (snd p)") 1
  , assertRuleId "UseUncurry: correct rule id" [useUncurryRule]
      (withPrelude "x = f (fst p) (snd p)") "UseUncurry"
  , assertSuggestion "UseUncurry: suggests uncurry" [useUncurryRule]
      (withPrelude "x = f (fst p) (snd p)") "uncurry f p"
  , assertWarningCount "UseUncurry: no fire on different args" [useUncurryRule]
      (withPrelude "x = f (fst p) (snd q)") 0
  , assertWarningCount "UseUncurry: no fire without import" [useUncurryRule]
      (withoutImports "x = f (fst p) (snd p)") 0
  ]

-- ============================================================================
-- UseFromJust Tests
-- ============================================================================

useFromJustTests :: Array TestResult
useFromJustTests =
  [ assertWarningCount "UseFromJust: fromMaybe d (Just x)" [useFromJustRule]
      (withPrelude "x = fromMaybe 0 (Just 1)") 1
  , assertRuleId "UseFromJust: correct rule id" [useFromJustRule]
      (withPrelude "x = fromMaybe 0 (Just 1)") "UseFromJust"
  , assertSuggestion "UseFromJust: simplifies Just" [useFromJustRule]
      (withPrelude "x = fromMaybe 0 (Just 1)") "1"
  , assertWarningCount "UseFromJust: fromMaybe d Nothing" [useFromJustRule]
      (withPrelude "x = fromMaybe 0 Nothing") 1
  , assertSuggestion "UseFromJust: simplifies Nothing" [useFromJustRule]
      (withPrelude "x = fromMaybe 0 Nothing") "0"
  , assertWarningCount "UseFromJust: no fire without import" [useFromJustRule]
      (withoutImports "x = fromMaybe 0 (Just 1)") 0
  ]

-- ============================================================================
-- UseHead Tests
-- ============================================================================

useHeadTests :: Array TestResult
useHeadTests =
  [ assertWarningCount "UseHead: xs !! 0" [useHeadRule]
      (withPrelude "x = xs !! 0") 1
  , assertRuleId "UseHead: correct rule id" [useHeadRule]
      (withPrelude "x = xs !! 0") "UseHead"
  , assertSuggestion "UseHead: suggests head" [useHeadRule]
      (withPrelude "x = xs !! 0") "head"
  , assertWarningCount "UseHead: no fire on xs !! 1" [useHeadRule]
      (withPrelude "x = xs !! 1") 0
  , assertWarningCount "UseHead: no fire without import" [useHeadRule]
      (withoutImports "x = xs !! 0") 0
  ]

-- ============================================================================
-- UseElemIndex Tests
-- ============================================================================

useElemIndexTests :: Array TestResult
useElemIndexTests =
  [ assertWarningCount "UseElemIndex: findIndex (\\x -> x == a) xs" [useElemIndexRule]
      (withPrelude "x = findIndex (\\y -> y == a) xs") 1
  , assertRuleId "UseElemIndex: correct rule id" [useElemIndexRule]
      (withPrelude "x = findIndex (\\y -> y == a) xs") "UseElemIndex"
  , assertSuggestion "UseElemIndex: suggests elemIndex" [useElemIndexRule]
      (withPrelude "x = findIndex (\\y -> y == a) xs") "elemIndex a xs"
  , assertWarningCount "UseElemIndex: no fire on other predicate" [useElemIndexRule]
      (withPrelude "x = findIndex (\\y -> y > a) xs") 0
  , assertWarningCount "UseElemIndex: no fire without import" [useElemIndexRule]
      (withoutImports "x = findIndex (\\y -> y == a) xs") 0
  ]

-- ============================================================================
-- UseBreak Tests
-- ============================================================================

useBreakTests :: Array TestResult
useBreakTests =
  [ assertWarningCount "UseBreak: span (not p) xs" [useBreakRule]
      (withPrelude "x = span (not p) xs") 1
  , assertRuleId "UseBreak: correct rule id" [useBreakRule]
      (withPrelude "x = span (not p) xs") "UseBreak"
  , assertSuggestion "UseBreak: suggests break" [useBreakRule]
      (withPrelude "x = span (not p) xs") "break p xs"
  , assertWarningCount "UseBreak: no fire without import" [useBreakRule]
      (withoutImports "x = span (not p) xs") 0
  ]

-- ============================================================================
-- UseSpan Tests
-- ============================================================================

useSpanTests :: Array TestResult
useSpanTests =
  [ assertWarningCount "UseSpan: break (not p) xs" [useSpanRule]
      (withPrelude "x = break (not p) xs") 1
  , assertRuleId "UseSpan: correct rule id" [useSpanRule]
      (withPrelude "x = break (not p) xs") "UseSpan"
  , assertSuggestion "UseSpan: suggests span" [useSpanRule]
      (withPrelude "x = break (not p) xs") "span p xs"
  , assertWarningCount "UseSpan: no fire without import" [useSpanRule]
      (withoutImports "x = break (not p) xs") 0
  ]

-- ============================================================================
-- UseDollar Tests
-- ============================================================================

useDollarTests :: Array TestResult
useDollarTests =
  [ assertWarningCount "UseDollar: map f (filter g xs)" [useDollarRule]
      (withPrelude "x = map f (filter g xs)") 1
  , assertRuleId "UseDollar: correct rule id" [useDollarRule]
      (withPrelude "x = map f (filter g xs)") "UseDollar"
  , assertSuggestion "UseDollar: suggests $" [useDollarRule]
      (withPrelude "x = map f (filter g xs)") "map f $ filter g xs"
  , assertWarningCount "UseDollar: no fire on atomic arg" [useDollarRule]
      (withPrelude "x = f (1)") 0
  ]

-- ============================================================================
-- UseUnwrap Tests
-- ============================================================================

useUnwrapTests :: Array TestResult
useUnwrapTests =
  [ assertRuleId "UseUnwrap: correct rule id" [useUnwrapRule]
      (withPrelude "x = unwrap foo") "UseUnwrap"
  , assertSuggestion "UseUnwrap: suggests un ?Constructor" [useUnwrapRule]
      (withPrelude "x = unwrap foo") "un ?Constructor foo"
  , assertWarningCount "UseUnwrap: unwrap partial" [useUnwrapRule]
      (withPrelude "x = unwrap") 1
  , assertSuggestion "UseUnwrap: unwrap partial suggestion" [useUnwrapRule]
      (withPrelude "x = unwrap") "un ?Constructor"
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
-- MonadLaw Tests
-- ============================================================================

monadLawTests :: Array TestResult
monadLawTests =
  [ assertWarningCount "MonadLaw: f =<< pure a" [monadLawRule]
      (withPrelude "x = show =<< pure 42") 1
  , assertRuleId "MonadLaw: correct rule id" [monadLawRule]
      (withPrelude "x = show =<< pure 42") "MonadLaw"
  , assertSuggestion "MonadLaw: suggests f a" [monadLawRule]
      (withPrelude "x = show =<< pure 42") "show 42"
  , assertWarningCount "MonadLaw: pure =<< m" [monadLawRule]
      (withPrelude "x = pure =<< Just 1") 1
  , assertSuggestion "MonadLaw: pure =<< m suggests m" [monadLawRule]
      (withPrelude "x = pure =<< Just 1") "Just 1"
  ]

-- ============================================================================
-- EvaluateBool Tests
-- ============================================================================

evaluateBoolTests :: Array TestResult
evaluateBoolTests =
  [ assertWarningCount "EvaluateBool: true && x" [evaluateBoolRule]
      (withPrelude "x = true && foo") 1
  , assertSuggestion "EvaluateBool: true && x -> x" [evaluateBoolRule]
      (withPrelude "x = true && foo") "foo"
  , assertWarningCount "EvaluateBool: false && x" [evaluateBoolRule]
      (withPrelude "x = false && foo") 1
  , assertSuggestion "EvaluateBool: false && x -> false" [evaluateBoolRule]
      (withPrelude "x = false && foo") "false"
  , assertWarningCount "EvaluateBool: true || x" [evaluateBoolRule]
      (withPrelude "x = true || foo") 1
  , assertSuggestion "EvaluateBool: true || x -> true" [evaluateBoolRule]
      (withPrelude "x = true || foo") "true"
  , assertWarningCount "EvaluateBool: false || x" [evaluateBoolRule]
      (withPrelude "x = false || foo") 1
  , assertSuggestion "EvaluateBool: false || x -> x" [evaluateBoolRule]
      (withPrelude "x = false || foo") "foo"
  , assertWarningCount "EvaluateBool: x && true" [evaluateBoolRule]
      (withPrelude "x = foo && true") 1
  , assertWarningCount "EvaluateBool: x || false" [evaluateBoolRule]
      (withPrelude "x = foo || false") 1
  ]

-- ============================================================================
-- UseFoldBool Tests
-- ============================================================================

useFoldBoolTests :: Array TestResult
useFoldBoolTests =
  [ assertWarningCount "UseFoldBool: foldr (&&) true" [useFoldBoolRule]
      (withPrelude "x = foldr (&&) true xs") 1
  , assertSuggestion "UseFoldBool: foldr (&&) true -> and" [useFoldBoolRule]
      (withPrelude "x = foldr (&&) true xs") "and"
  , assertWarningCount "UseFoldBool: foldr (||) false" [useFoldBoolRule]
      (withPrelude "x = foldr (||) false xs") 1
  , assertSuggestion "UseFoldBool: foldr (||) false -> or" [useFoldBoolRule]
      (withPrelude "x = foldr (||) false xs") "or"
  ]

-- ============================================================================
-- AlternativeLaw Tests
-- ============================================================================

alternativeLawTests :: Array TestResult
alternativeLawTests =
  [ assertWarningCount "AlternativeLaw: empty <|> x" [alternativeLawRule]
      (withAlt "x = empty <|> foo") 1
  , assertSuggestion "AlternativeLaw: empty <|> x -> x" [alternativeLawRule]
      (withAlt "x = empty <|> foo") "foo"
  , assertWarningCount "AlternativeLaw: x <|> empty" [alternativeLawRule]
      (withAlt "x = foo <|> empty") 1
  , assertSuggestion "AlternativeLaw: x <|> empty -> x" [alternativeLawRule]
      (withAlt "x = foo <|> empty") "foo"
  , assertWarningCount "AlternativeLaw: Nothing <|> x" [alternativeLawRule]
      (withAlt "x = Nothing <|> foo") 1
  ]

-- ============================================================================
-- EvaluateEither Tests
-- ============================================================================

evaluateEitherTests :: Array TestResult
evaluateEitherTests =
  [ assertWarningCount "EvaluateEither: either f g (Left x)" [evaluateEitherRule]
      (withPrelude "x = either show identity (Left 42)") 1
  , assertSuggestion "EvaluateEither: either f g (Left x) -> f x" [evaluateEitherRule]
      (withPrelude "x = either show identity (Left 42)") "show 42"
  , assertWarningCount "EvaluateEither: either f g (Right y)" [evaluateEitherRule]
      (withPrelude "x = either show identity (Right \"hi\")") 1
  , assertSuggestion "EvaluateEither: either f g (Right y) -> g y" [evaluateEitherRule]
      (withPrelude "x = either show identity (Right \"hi\")") "identity \"hi\""
  ]

-- ============================================================================
-- UseCaseOf Tests
-- ============================================================================

useCaseOfTests :: Array TestResult
useCaseOfTests =
  -- Positive cases: should detect
  [ assertWarningCount "UseCaseOf: two clauses" [useCaseOfRule]
      (withPrelude "foo True = 1\nfoo False = 0") 1
  , assertRuleId "UseCaseOf: correct rule id" [useCaseOfRule]
      (withPrelude "foo True = 1\nfoo False = 0") "UseCaseOf"
  , assertSuggestion "UseCaseOf: suggestion format" [useCaseOfRule]
      (withPrelude "foo True = 1\nfoo False = 0") "foo = case _ of\n  True -> 1\n  False -> 0"
  , assertWarningCount "UseCaseOf: three clauses" [useCaseOfRule]
      (withPrelude "bar Nothing = 0\nbar (Just 0) = 1\nbar (Just _) = 2") 1
  , assertWarningCount "UseCaseOf: two args" [useCaseOfRule]
      (withPrelude "add True True = 2\nadd True False = 1\nadd False True = 1\nadd False False = 0") 1
  , assertSuggestion "UseCaseOf: two args suggestion" [useCaseOfRule]
      (withPrelude "add True True = 2\nadd True False = 1\nadd False True = 1\nadd False False = 0") "add = case _, _ of\n  True, True -> 2\n  True, False -> 1\n  False, True -> 1\n  False, False -> 0"
  
  -- Clauses with where bindings
  , assertWarningCount "UseCaseOf: clause with where binding" [useCaseOfRule]
      (withPrelude "foo x = go x\n  where\n  isJoin True = x\n    where x = 1\n  isJoin False = 0") 1
  , assertSuggestion "UseCaseOf: clause with where binding suggestion" [useCaseOfRule]
      (withPrelude "foo x = go x\n  where\n  isJoin True = x\n    where x = 1\n  isJoin False = 0") "isJoin = case _ of\n    True ->\n        x\n        where\n        x = 1\n    False -> 0"

  -- Regression test: nested case expression should preserve multiline formatting
  , assertWarningCount "UseCaseOf: nested case expression" [useCaseOfRule]
      (withPrelude "extractFn (Just x) =\n  case x of\n    1 -> \"one\"\n    _ -> \"other\"\nextractFn Nothing = \"nothing\"") 1
  , assertSuggestion "UseCaseOf: nested case should preserve newlines" [useCaseOfRule]
      (withPrelude "extractFn (Just x) =\n  case x of\n    1 -> \"one\"\n    _ -> \"other\"\nextractFn Nothing = \"nothing\"") "extractFn = case _ of\n  (Just x) ->\n    case x of\n      1 -> \"one\"\n      _ -> \"other\"\n  Nothing -> \"nothing\""

  -- Negative cases: should NOT detect
  , assertWarningCount "UseCaseOf: single clause" [useCaseOfRule]
      (withPrelude "foo x = x + 1") 0
  , assertWarningCount "UseCaseOf: no binders" [useCaseOfRule]
      (withPrelude "foo = 42\nbar = 43") 0
  , assertWarningCount "UseCaseOf: guarded" [useCaseOfRule]
      (withPrelude "foo x | x > 0 = 1\nfoo x = 0") 0
  ]

-- ============================================================================
-- FlattenCase Tests
-- ============================================================================

flattenCaseTests :: Array TestResult
flattenCaseTests =
  -- Positive cases: should detect
  [ assertWarningCount "FlattenCase: two levels" [flattenCaseRule]
      (withPrelude "foo x y = case x of\n  Just a -> case y of\n    Just b -> a + b\n    _ -> 0\n  _ -> 0") 1
  , assertRuleId "FlattenCase: correct rule id" [flattenCaseRule]
      (withPrelude "foo x y = case x of\n  Just a -> case y of\n    Just b -> a + b\n    _ -> 0\n  _ -> 0") "FlattenCase"
  , assertSuggestion "FlattenCase: suggestion format" [flattenCaseRule]
      (withPrelude "foo x y = case x of\n  Just a -> case y of\n    Just b -> a + b\n    _ -> 0\n  _ -> 0") "case x, y of\n  Just a, Just b -> a + b\n  _, _ -> 0"
  
  -- Negative cases: should NOT detect
  , assertWarningCount "FlattenCase: single level" [flattenCaseRule]
      (withPrelude "foo x = case x of\n  Just a -> a\n  _ -> 0") 0
  , assertWarningCount "FlattenCase: different fallbacks" [flattenCaseRule]
      (withPrelude "foo x y = case x of\n  Just a -> case y of\n    Just b -> a + b\n    _ -> 1\n  _ -> 0") 0
  ]

-- ============================================================================
-- UsePatternGuards Tests
-- ============================================================================

usePatternGuardsTests :: Array TestResult
usePatternGuardsTests =
  -- Positive cases: should detect
  [ assertWarningCount "UsePatternGuards: two levels" [usePatternGuardsRule]
      (withPrelude "foo x y = case x of\n  Just a -> case y of\n    Just b -> a + b\n    _ -> 0\n  _ -> 0") 1
  , assertRuleId "UsePatternGuards: correct rule id" [usePatternGuardsRule]
      (withPrelude "foo x y = case x of\n  Just a -> case y of\n    Just b -> a + b\n    _ -> 0\n  _ -> 0") "UsePatternGuards"
  , assertWarningCount "UsePatternGuards: three levels" [usePatternGuardsRule]
      (withPrelude "foo x y z = case x of\n  Just a -> case y of\n    Just b -> case z of\n      Just c -> a + b + c\n      _ -> 0\n    _ -> 0\n  _ -> 0") 2
  
  -- Negative cases: should NOT detect
  , assertWarningCount "UsePatternGuards: single level" [usePatternGuardsRule]
      (withPrelude "foo x = case x of\n  Just a -> a\n  _ -> 0") 0
  , assertWarningCount "UsePatternGuards: different fallbacks" [usePatternGuardsRule]
      (withPrelude "foo x y = case x of\n  Just a -> case y of\n    Just b -> a + b\n    _ -> 1\n  _ -> 0") 0
  
  -- Fix test: verifies correct indentation relative to original branch position
  , assertFix "UsePatternGuards: fix with proper indentation" [usePatternGuardsRule]
      (withPrelude "foo x y = case x of\n  Just a -> case y of\n    Just b -> a + b\n    _ -> 0\n  _ -> 0")
      (withPrelude "foo x y = case x of\n  Just a\n    | Just b <- y -> a + b\n  _ -> 0")
  ]

-- ============================================================================
-- RedundantParens Tests
-- ============================================================================

redundantParensTests :: Array TestResult
redundantParensTests =
  -- Positive cases: should detect redundant parens
  [ assertWarningCount "RedundantParens: nested parens ((x))" [redundantParensRule]
      (withPrelude "x = ((1))") 1
  , assertRuleId "RedundantParens: correct rule id" [redundantParensRule]
      (withPrelude "x = ((1))") "RedundantParens"
  , assertSuggestion "RedundantParens: nested parens suggestion" [redundantParensRule]
      (withPrelude "x = ((1))") "(1)"
  
  -- Function arguments with atomic expressions
  , assertWarningCount "RedundantParens: literal in arg" [redundantParensRule]
      (withPrelude "x = show (42)") 1
  , assertSuggestion "RedundantParens: literal in arg suggestion" [redundantParensRule]
      (withPrelude "x = show (42)") "42"
  , assertWarningCount "RedundantParens: identifier in arg" [redundantParensRule]
      (withPrelude "x y = show (y)") 1
  , assertWarningCount "RedundantParens: string in arg" [redundantParensRule]
      (withPrelude "x = show (\"hi\")") 1
  , assertWarningCount "RedundantParens: array in arg" [redundantParensRule]
      (withPrelude "x = length ([1, 2])") 1
  , assertWarningCount "RedundantParens: record in arg" [redundantParensRule]
      (withPrelude "x = show ({ a: 1 })") 1
  
  -- Multiple redundant args
  , assertWarningCount "RedundantParens: two redundant args" [redundantParensRule]
      (withPrelude "x = add (1) (2)") 2
  
  -- Array elements
  , assertWarningCount "RedundantParens: array element literal" [redundantParensRule]
      (withPrelude "x = [(1), (2)]") 2
  , assertWarningCount "RedundantParens: array mixed" [redundantParensRule]
      (withPrelude "x = [(1), 2, (3)]") 2
  , assertWarningCount "RedundantParens: no fire on complex array element" [redundantParensRule]
      (withPrelude "x = [(1 + 2)]") 0
  
  -- Negative cases: should NOT detect
  , assertWarningCount "RedundantParens: no fire on precedence (1+2)*3" [redundantParensRule]
      (withPrelude "x = (1 + 2) * 3") 0
  , assertWarningCount "RedundantParens: no fire on complex arg" [redundantParensRule]
      (withPrelude "x = show (1 + 2)") 0
  , assertWarningCount "RedundantParens: no fire on lambda arg" [redundantParensRule]
      (withPrelude "x = map (\\y -> y + 1) [1]") 0
  , assertWarningCount "RedundantParens: no fire on negate arg" [redundantParensRule]
      (withPrelude "x f = f (-1)") 0
  , assertWarningCount "RedundantParens: no fire on section" [redundantParensRule]
      (withPrelude "x = (_ + 1)") 0
  , assertWarningCount "RedundantParens: no fire on if" [redundantParensRule]
      (withPrelude "x = (if true then 1 else 2)") 0
  , assertWarningCount "RedundantParens: no fire on let" [redundantParensRule]
      (withPrelude "x = (let y = 1 in y)") 0
  
  -- Outer parens on RHS (simple cases)
  , assertWarningCount "RedundantParens: outer parens on literal RHS" [redundantParensRule]
      (withPrelude "x = (42)") 1
  , assertWarningCount "RedundantParens: outer parens on app RHS" [redundantParensRule]
      (withPrelude "x = (show 1)") 1
  , assertWarningCount "RedundantParens: outer parens on op RHS" [redundantParensRule]
      (withPrelude "x = (1 + 2)") 1
  
  -- If branches
  , assertWarningCount "RedundantParens: if condition" [redundantParensRule]
      (withPrelude "x = if (true) then 1 else 2") 1
  , assertWarningCount "RedundantParens: if then branch" [redundantParensRule]
      (withPrelude "x = if true then (1) else 2") 1
  
  -- Case branches
  , assertWarningCount "RedundantParens: case scrutinee" [redundantParensRule]
      (withPrelude "x = case (1) of y -> y") 1
  , assertWarningCount "RedundantParens: case body" [redundantParensRule]
      (withPrelude "x = case 1 of y -> (y)") 1
  
  -- Let expression
  , assertWarningCount "RedundantParens: let binding" [redundantParensRule]
      (withPrelude "x = let y = (1) in y") 1
  , assertWarningCount "RedundantParens: let body" [redundantParensRule]
      (withPrelude "x = let y = 1 in (y)") 1
  
  -- Record fields
  , assertWarningCount "RedundantParens: record field" [redundantParensRule]
      (withPrelude "x = { a: (1) }") 1
  , assertWarningCount "RedundantParens: multiple record fields" [redundantParensRule]
      (withPrelude "x = { a: (1), b: (2) }") 2
  
  -- Pattern (binder) cases
  , assertWarningCount "RedundantParens: nested parens in pattern" [redundantParensRule]
      (withPrelude "f ((x)) = x") 1
  , assertSuggestion "RedundantParens: nested parens in pattern suggestion" [redundantParensRule]
      (withPrelude "f ((x)) = x") "(x)"
  , assertWarningCount "RedundantParens: atomic pattern in constructor arg" [redundantParensRule]
      (withPrelude "f (Just (x)) = x\nf Nothing = 0") 1
  , assertSuggestion "RedundantParens: constructor arg suggestion" [redundantParensRule]
      (withPrelude "f (Just (x)) = x\nf Nothing = 0") "x"
  , assertWarningCount "RedundantParens: wildcard in constructor" [redundantParensRule]
      (withPrelude "f (Just (_)) = 1\nf Nothing = 0") 1
  , assertWarningCount "RedundantParens: literal in constructor" [redundantParensRule]
      (withPrelude "f (Just (1)) = true\nf _ = false") 1
  , assertWarningCount "RedundantParens: array pattern element" [redundantParensRule]
      (withPrelude "f [(x), (y)] = x + y\nf _ = 0") 2
  
  -- Negative pattern cases
  , assertWarningCount "RedundantParens: no fire on constructor with args" [redundantParensRule]
      (withPrelude "f (Just (Right x)) = x\nf _ = 0") 0
  , assertWarningCount "RedundantParens: no fire on typed pattern" [redundantParensRule]
      (withPrelude "f (x :: Int) = x") 0
  , assertWarningCount "RedundantParens: no fire on negated number pattern" [redundantParensRule]
      (withPrelude "f (Just (-1)) = true\nf _ = false") 0
  
  -- Top-level case pattern parens
  , assertWarningCount "RedundantParens: top-level case pattern" [redundantParensRule]
      (withPrelude "f = case _ of\n  (Just x) -> x\n  Nothing -> 0") 1
  , assertSuggestion "RedundantParens: top-level case pattern suggestion" [redundantParensRule]
      (withPrelude "f = case _ of\n  (Just x) -> x\n  Nothing -> 0") "Just x"
  , assertWarningCount "RedundantParens: top-level constructor pattern" [redundantParensRule]
      (withPrelude "f = case _ of\n  (Foo a b) -> a\n  _ -> 0") 1
  , assertWarningCount "RedundantParens: no fire on cons pattern" [redundantParensRule]
      (withPrelude "f = case _ of\n  (x : xs) -> x\n  _ -> 0") 0
  , assertWarningCount "RedundantParens: no fire on typed case pattern" [redundantParensRule]
      (withPrelude "f = case _ of\n  (x :: Int) -> x") 0
  
  -- Type-level redundant parens
  , assertWarningCount "RedundantParens: nested type parens" [redundantParensRule]
      (withPrelude "x :: ((Int))\nx = 1") 1
  , assertSuggestion "RedundantParens: nested type parens suggestion" [redundantParensRule]
      (withPrelude "x :: ((Int))\nx = 1") "(Int)"
  , assertWarningCount "RedundantParens: type app with atomic arg" [redundantParensRule]
      (withPrelude "x :: Maybe (Int)\nx = Nothing") 1
  , assertSuggestion "RedundantParens: type app atomic arg suggestion" [redundantParensRule]
      (withPrelude "x :: Maybe (Int)\nx = Nothing") "Int"
  , assertWarningCount "RedundantParens: type app with type var" [redundantParensRule]
      (withPrelude "x :: forall a. Maybe (a)\nx = Nothing") 1
  , assertWarningCount "RedundantParens: arrow left simple" [redundantParensRule]
      (withPrelude "x :: (Int) -> String\nx = show") 1
  , assertWarningCount "RedundantParens: record field type" [redundantParensRule]
      (withPrelude "type R = { x :: (Int) }") 1
  
  -- Type-level negative cases
  , assertWarningCount "RedundantParens: no fire on function type in arg" [redundantParensRule]
      (withPrelude "x :: (Int -> Int) -> Int\nx f = f 1") 0
  , assertWarningCount "RedundantParens: no fire on forall in arg" [redundantParensRule]
      (withPrelude "x :: (forall a. a -> a) -> Int\nx f = f 1") 0
  , assertWarningCount "RedundantParens: no fire on constrained type" [redundantParensRule]
      (withPrelude "x :: (Show a) => a -> String\nx = show") 0
  ]

-- | Helper for Alternative imports
withAlt :: String -> String
withAlt code = """
module Test where
import Prelude
import Control.Alternative (empty, (<|>))
import Data.Maybe (Maybe(..), Nothing, Just)
""" <> code

-- | All rules for golden testing
allRulesForGolden :: Array Rule
allRulesForGolden =
  [ mapIdentityRule
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

goldenTests :: Effect (Array TestResult)
goldenTests = traverse identity
  -- MapIdentity: map identity x -> x
  [ assertFixFormatted "Golden: MapIdentity fix" [mapIdentityRule]
      (withPrelude "x = map identity [ 1, 2, 3 ]")
      (withPrelude "x = [ 1, 2, 3 ]")
  
  -- MapFusion: map f (map g x) -> map (f <<< g) x
  , assertFixFormatted "Golden: MapFusion fix" [mapFusionRule]
      (withPrelude "x = map show (map (_ + 1) [ 1, 2, 3 ])")
      (withPrelude "x = map (show <<< (_ + 1)) [ 1, 2, 3 ]")
  
  -- UseTraverse: sequence (map f x) -> mapM f x
  , assertFixFormatted "Golden: UseTraverse fix" [useTraverseRule]
      (withPrelude "x f xs = sequence (map f xs)")
      (withPrelude "x f xs = mapM f xs")
  
  -- ConcatMap: join (map f x) -> (=<<) f x
  , assertFixFormatted "Golden: ConcatMap fix" [concatMapRule]
      (withPrelude "x f xs = join (map f xs)")
      (withPrelude "x f xs = (=<<) f xs")
  
  -- NotEqual: not (a == b) -> a /= b
  , assertFixFormatted "Golden: NotEqual fix" [notEqualRule]
      (withPrelude "x a b = not (a == b)")
      (withPrelude "x a b = a /= b")
  
  -- EtaReduce: \x -> f x -> f
  , assertFixFormatted "Golden: EtaReduce fix" [etaReduceRule]
      (withPrelude "x = \\y -> show y")
      (withPrelude "x = show")
  
  -- RedundantBind: pure x >>= f -> f x
  , assertFixFormatted "Golden: RedundantBind fix" [redundantBindRule]
      (withPrelude "x y f = pure y >>= f")
      (withPrelude "x y f = f y")
  
  -- RedundantIf: if a then true else false -> a
  , assertFixFormatted "Golden: RedundantIf fix" [redundantIfRule]
      (withPrelude "x a = if a then true else false")
      (withPrelude "x a = a")
  
  -- UseVoid: a *> pure unit -> void a
  , assertFixFormatted "Golden: UseVoid fix" [useVoidRule]
      (withPrelude "x a = a *> pure unit")
      (withPrelude "x a = void a")
  
  -- UseJoin: x >>= identity -> join x
  , assertFixFormatted "Golden: UseJoin fix" [useJoinRule]
      (withPrelude "x m = m >>= identity")
      (withPrelude "x m = join m")
  
  -- UseAny: or (map f x) -> any f x
  , assertFixFormatted "Golden: UseAny fix" [useAnyRule]
      (withPrelude "x f xs = or (map f xs)")
      (withPrelude "x f xs = any f xs")
  
  -- UseAll: and (map f x) -> all f x
  , assertFixFormatted "Golden: UseAll fix" [useAllRule]
      (withPrelude "x f xs = and (map f xs)")
      (withPrelude "x f xs = all f xs")
  
  -- BooleanSimplify: x == true -> x
  , assertFixFormatted "Golden: BooleanSimplify fix" [booleanSimplifyRule]
      (withPrelude "x a = a == true")
      (withPrelude "x a = a")
  
  -- CollapseLambdas: \x -> \y -> body -> \x y -> body
  , assertFixFormatted "Golden: CollapseLambdas fix" [collapseLambdasRule]
      (withPrelude "x = \\a -> \\b -> a + b")
      (withPrelude "x = \\a b -> a + b")
  
  -- UseConst: \_ -> y -> const y
  , assertFixFormatted "Golden: UseConst fix" [useConstRule]
      (withPrelude "x = \\_ -> 42")
      (withPrelude "x = const 42")
  
  -- UseNull: length xs == 0 -> null xs
  , assertFixFormatted "Golden: UseNull fix" [useNullRule]
      (withPrelude "x xs = length xs == 0")
      (withPrelude "x xs = null xs")
  
  -- UseFromMaybe: maybe def identity -> fromMaybe def
  , assertFixFormatted "Golden: UseFromMaybe fix" [useFromMaybeRule]
      (withPrelude "x def = maybe def identity")
      (withPrelude "x def = fromMaybe def")
  
  -- UseIsJust: maybe false (const true) -> isJust
  , assertFixFormatted "Golden: UseIsJust fix" [useIsJustRule]
      (withPrelude "x = maybe false (const true)")
      (withPrelude "x = isJust")
  
  -- UseIsNothing: maybe true (const false) -> isNothing
  , assertFixFormatted "Golden: UseIsNothing fix" [useIsNothingRule]
      (withPrelude "x = maybe true (const false)")
      (withPrelude "x = isNothing")
  
  -- UseWhen: if cond then action else pure unit -> when cond action
  , assertFixFormatted "Golden: UseWhen fix" [useWhenRule]
      (withPrelude "x cond action = if cond then action else pure unit")
      (withPrelude "x cond action = when cond action")
  
  -- UseUnless: if cond then pure unit else action -> unless cond action
  , assertFixFormatted "Golden: UseUnless fix" [useUnlessRule]
      (withPrelude "x cond action = if cond then pure unit else action")
      (withPrelude "x cond action = unless cond action")
  
  -- RedundantFlip: flip (flip f) -> f
  , assertFixFormatted "Golden: RedundantFlip fix" [redundantFlipRule]
      (withPrelude "x f = flip (flip f)")
      (withPrelude "x f = f")
  
  -- UseNotElem: not (elem x xs) -> notElem x xs
  , assertFixFormatted "Golden: UseNotElem fix" [useNotElemRule]
      (withPrelude "x a xs = not (elem a xs)")
      (withPrelude "x a xs = notElem a xs")
  
  -- UseMinMax: if a > b then a else b -> max a b
  , assertFixFormatted "Golden: UseMinMax fix" [useMinMaxRule]
      (withPrelude "x a b = if a > b then a else b")
      (withPrelude "x a b = max a b")
  
  -- MonoidIdentity: mempty <> x -> x
  , assertFixFormatted "Golden: MonoidIdentity fix" [monoidIdentityRule]
      (withPrelude "x xs = mempty <> xs")
      (withPrelude "x xs = xs")
  
  -- UseFold: foldMap identity -> fold
  , assertFixFormatted "Golden: UseFold fix" [useFoldRule]
      (withPrelude "x = foldMap identity")
      (withPrelude "x = fold")
  
  -- UseSequence: traverse identity -> sequence
  , assertFixFormatted "Golden: UseSequence fix" [useSequenceRule]
      (withPrelude "x = traverse identity")
      (withPrelude "x = sequence")
  
  -- RedundantNot: not (not x) -> x
  , assertFixFormatted "Golden: RedundantNot fix" [redundantNotRule]
      (withPrelude "x a = not (not a)")
      (withPrelude "x a = a")
  
  -- UseFstSnd: \(Tuple x _) -> x -> fst
  , assertFixFormatted "Golden: UseFstSnd fix" [useFstSndRule]
      (withPrelude "x = \\(Tuple a b) -> a")
      (withPrelude "x = fst")
  
  -- UseMapMaybe: catMaybes (map f x) -> mapMaybe f x
  , assertFixFormatted "Golden: UseMapMaybe fix" [useMapMaybeRule]
      (withPrelude "x f xs = catMaybes (map f xs)")
      (withPrelude "x f xs = mapMaybe f xs")
  
  -- UseApplicative: pure f <*> x -> f <$> x
  , assertFixFormatted "Golden: UseApplicative fix" [useApplicativeRule]
      (withPrelude "x f y = pure f <*> y")
      (withPrelude "x f y = f <$> y")
  
  -- UseBindFlip: join (map f x) -> x >>= f
  , assertFixFormatted "Golden: UseBindFlip fix" [useBindFlipRule]
      (withPrelude "x f y = join (map f y)")
      (withPrelude "x f y = y >>= f")
  
  -- UseFor: flip traverse -> for
  , assertFixFormatted "Golden: UseFor fix" [useForRule]
      (withPrelude "x = flip traverse")
      (withPrelude "x = for")
  
  -- RedundantGuard: guard true -> pure unit
  , assertFixFormatted "Golden: RedundantGuard fix" [redundantGuardRule]
      (withPrelude "x = guard true")
      (withPrelude "x = pure unit")
  
  -- UseComparing: \a b -> compare (f a) (f b) -> comparing f
  , assertFixFormatted "Golden: UseComparing fix" [useComparingRule]
      (withPrelude "x f = \\a b -> compare (f a) (f b)")
      (withPrelude "x f = comparing f")
  
  -- UseOn: \a b -> f (g a) (g b) -> on f g
  , assertFixFormatted "Golden: UseOn fix" [useOnRule]
      (withPrelude "x f g = \\a b -> f (g a) (g b)")
      (withPrelude "x f g = on f g")
  
  -- UseFindMap: head (mapMaybe f x) -> findMap f x
  , assertFixFormatted "Golden: UseFindMap fix" [useFindMapRule]
      (withPrelude "x f xs = head (mapMaybe f xs)")
      (withPrelude "x f xs = findMap f xs")
  
  -- LetToDo: nested let...in -> do notation (do on previous line)
  , pure $ assertFix "Golden: LetToDo nested fix" [letToDoRule]
      (withPrelude "x f =\n  case f of\n    Just y ->\n      let\n        z = y + 1\n      in z * 2\n    Nothing -> 0")
      (withPrelude "x f =\n  case f of\n    Just y -> do\n      let\n        z = y + 1\n      z * 2\n    Nothing -> 0")

  -- LetToDo: should NOT fire on do-let (no "in" keyword)
  , pure $ assertWarningCount "Golden: LetToDo no fire on do-let" [letToDoRule]
      (withPrelude "x f =\n  case f of\n    Just y -> do\n      let\n        z = y + 1\n      [ z ]\n    Nothing -> []")
      0

  -- UsePatternGuards: should preserve do-let body structure
  , pure $ assertFix "Golden: UsePatternGuards preserves do-let" [usePatternGuardsRule]
      (withPrelude "checkExpr expr =\n  case expr of\n    Just mapArgs ->\n      case toArray mapArgs of\n        [ x, y ] -> do\n          let\n            f = show x\n            g = show y\n          [ f, g ]\n        _ -> []\n    _ -> []")
      (withPrelude "checkExpr expr =\n  case expr of\n    Just mapArgs\n      | [ x, y ] <- toArray mapArgs -> do\n          let\n            f = show x\n            g = show y\n          [ f, g ]\n    _ -> [ ]")
  
  -- UseFoldMap: should preserve multiline formatting in complex body
  , pure $ assertFix "Golden: UseFoldMap preserves multiline body" [useFoldMapRule]
      (withPrelude "checkExpr mx =\n  case mx of\n    Just p ->\n      let\n        pText = show p\n      in\n        [ pText ]\n    Nothing -> []")
      (withPrelude "checkExpr mx =\n  foldMap\n    ( \\p ->\n        let\n          pText = show p\n        in\n          [ pText ]\n    )\n    mx")
  
  -- UseEitherMap: either Left (Right <<< f) -> map f
  , pure $ assertFix "Golden: UseEitherMap basic" [useEitherMapRule]
      (withPrelude "x = either Left (Right <<< show)")
      (withPrelude "x = map show")
  
  -- UseConst: \_ -> multiline body -> const (multiline body)
  , pure $ assertFix "Golden: UseConst preserves multiline body" [useConstRule]
      (withPrelude "x =\n  foldMap\n    ( \\_ ->\n        [ LintWarning\n            { ruleId: RuleId \"UseEitherMap\"\n            , message: WarningMessage \"either Left (Right <<< f) can be simplified to map f\"\n            }\n        ]\n    )\n    secondArg")
      (withPrelude "x =\n  foldMap\n    ( const\n        [ LintWarning\n            { ruleId: RuleId \"UseEitherMap\"\n            , message: WarningMessage \"either Left (Right <<< f) can be simplified to map f\"\n            }\n        ]\n    )\n    secondArg")
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
  , { name: "map fusion with <$>", suggestion: "(f <<< g) <$> x" }
  , { name: "flipped map fusion", suggestion: "x <#> (g >>> f)" }
  , { name: "where clause", suggestion: "y + 1\n  where\n  y = 1" }
  ]

-- | Run all tidy formatting tests
runTidyTests :: Effect (Array TestResult)
runTidyTests = traverse (\t -> assertTidyFormatted ("Tidy: " <> t.name) t.suggestion) tidySuggestions
