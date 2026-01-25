module Test.Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Class.Console (log)
import Purelint.Fix (applyAllFixes)
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
import Purelint.Rule (Rule)
import Purelint.Runner (runRules)
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

-- | Helper: wrap code in a module with Prelude import
withPrelude :: String -> String
withPrelude code = "module Test where\nimport Prelude\n" <> code

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
