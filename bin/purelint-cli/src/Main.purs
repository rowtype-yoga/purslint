module Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String.CodeUnits as String
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Simple as FS
import Node.CLI as Process
import Purelint.Config (defaultConfig, filterRules, parseConfig)
import Purelint.Fix (applyAllFixes)
import Purelint.Rule (Rule)
import Purelint.Rules.BooleanSimplify (booleanSimplifyRule)
import Purelint.Rules.CollapseLambdas (collapseLambdasRule)
import Purelint.Rules.ConcatMap (concatMapRule)
import Purelint.Rules.EtaReduce (etaReduceRule)
import Purelint.Rules.EtaReduceDecl (etaReduceDeclRule)
import Purelint.Rules.AlternativeLaw (alternativeLawRule)
import Purelint.Rules.EvaluateBool (evaluateBoolRule)
import Purelint.Rules.EvaluateConst (evaluateConstRule)
import Purelint.Rules.EvaluateEither (evaluateEitherRule)
import Purelint.Rules.EvaluateFst (evaluateFstRule)
import Purelint.Rules.MapIdentity (mapIdentityRule)
import Purelint.Rules.FunctorLaw (functorLawRule)
import Purelint.Rules.LetToDo (letToDoRule)
import Purelint.Rules.LetToWhere (letToWhereRule)
import Purelint.Rules.MapFusion (mapFusionRule)
import Purelint.Rules.MonadLaw (monadLawRule)
import Purelint.Rules.MonoidIdentity (monoidIdentityRule)
import Purelint.Rules.NotEqual (notEqualRule)
import Purelint.Rules.NothingBind (nothingBindRule)
import Purelint.Rules.RedundantBind (redundantBindRule)
import Purelint.Rules.RedundantFlip (redundantFlipRule)
import Purelint.Rules.RedundantGuard (redundantGuardRule)
import Purelint.Rules.RedundantId (redundantIdRule)
import Purelint.Rules.RedundantIf (redundantIfRule)
import Purelint.Rules.RedundantNegate (redundantNegateRule)
import Purelint.Rules.RedundantNot (redundantNotRule)
import Purelint.Rules.RedundantReverse (redundantReverseRule)
import Purelint.Rules.UnlessNot (unlessNotRule)
import Purelint.Rules.UseAll (useAllRule)
import Purelint.Rules.UseAnd (useAndRule)
import Purelint.Rules.UseAny (useAnyRule)
import Purelint.Rules.UseApplicative (useApplicativeRule)
import Purelint.Rules.UseApplyFlipped (useApplyFlippedRule)
import Purelint.Rules.UseBimap (useBimapRule)
import Purelint.Rules.UseBindFlip (useBindFlipRule)
import Purelint.Rules.UseBreak (useBreakRule)
import Purelint.Rules.UseComparing (useComparingRule)
import Purelint.Rules.UseConst (useConstRule)
import Purelint.Rules.UseEitherMap (useEitherMapRule)
import Purelint.Rules.UseElemIndex (useElemIndexRule)
import Purelint.Rules.UseFindMap (useFindMapRule)
import Purelint.Rules.UseFold (useFoldRule)
import Purelint.Rules.UseFoldBool (useFoldBoolRule)
import Purelint.Rules.UseFoldMap (useFoldMapRule)
import Purelint.Rules.UseFoldMapId (useFoldMapIdRule)
import Purelint.Rules.UseFor (useForRule)
import Purelint.Rules.UseFromJust (useFromJustRule)
import Purelint.Rules.UseFromMaybe (useFromMaybeRule)
import Purelint.Rules.UseFstSnd (useFstSndRule)
import Purelint.Rules.UseGuard (useGuardRule)
import Purelint.Rules.UseGuardMaybe (useGuardMaybeRule)
import Purelint.Rules.UseHead (useHeadRule)
import Purelint.Rules.UseIsJust (useIsJustRule)
import Purelint.Rules.UseIsNothing (useIsNothingRule)
import Purelint.Rules.UseJoin (useJoinRule)
import Purelint.Rules.UseLastReverse (useLastReverseRule)
import Purelint.Rules.UseMapMaybe (useMapMaybeRule)
import Purelint.Rules.UseMaybeMap (useMaybeMapRule)
import Purelint.Rules.UseMinMax (useMinMaxRule)
import Purelint.Rules.UseMinimumSort (useMinimumSortRule)
import Purelint.Rules.UseNotElem (useNotElemRule)
import Purelint.Rules.UseNull (useNullRule)
import Purelint.Rules.UseOn (useOnRule)
import Purelint.Rules.UseOr (useOrRule)
import Purelint.Rules.UsePatternGuards (usePatternGuardsRule)
import Purelint.Rules.UseReplicate (useReplicateRule)
import Purelint.Rules.UseSequence (useSequenceRule)
import Purelint.Rules.UseSpan (useSpanRule)
import Purelint.Rules.UseTraverse (useTraverseRule)
import Purelint.Rules.UseTraverseSequence (useTraverseSequenceRule)
import Purelint.Rules.UseUncurry (useUncurryRule)
import Purelint.Rules.UseUnless (useUnlessRule)
import Purelint.Rules.UseUnwrap (useUnwrapRule)
import Purelint.Rules.UseVoid (useVoidRule)
import Purelint.Rules.UseWhen (useWhenRule)
import Purelint.Rules.UseZip (useZipRule)
import Purelint.Rules.WhenNot (whenNotRule)
import Purelint.Runner (runRules)
import Purelint.Types (LintResult(..), LintWarning(..), Severity(..), SourceCode(..), Suggestion(..))

-- | All available rules (76 total)
allRules :: Array Rule
allRules =
  [ alternativeLawRule
  , booleanSimplifyRule
  , collapseLambdasRule
  , concatMapRule
  , etaReduceRule
  , etaReduceDeclRule
  , evaluateBoolRule
  , evaluateConstRule
  , evaluateEitherRule
  , evaluateFstRule
  , mapIdentityRule
  , functorLawRule
  , letToDoRule
  , letToWhereRule
  , mapFusionRule
  , monadLawRule
  , monoidIdentityRule
  , notEqualRule
  , nothingBindRule
  , redundantBindRule
  , redundantFlipRule
  , redundantGuardRule
  , redundantIdRule
  , redundantIfRule
  , redundantNegateRule
  , redundantNotRule
  , redundantReverseRule
  , unlessNotRule
  , useAllRule
  , useAndRule
  , useAnyRule
  , useApplicativeRule
  , useApplyFlippedRule
  , useBimapRule
  , useBindFlipRule
  , useBreakRule
  , useComparingRule
  , useConstRule
  , useEitherMapRule
  , useElemIndexRule
  , useFindMapRule
  , useFoldRule
  , useFoldBoolRule
  , useFoldMapRule
  , useFoldMapIdRule
  , useForRule
  , useFromJustRule
  , useFromMaybeRule
  , useFstSndRule
  , useGuardRule
  , useGuardMaybeRule
  , useHeadRule
  , useIsJustRule
  , useIsNothingRule
  , useJoinRule
  , useLastReverseRule
  , useMapMaybeRule
  , useMaybeMapRule
  , useMinMaxRule
  , useMinimumSortRule
  , useNotElemRule
  , useNullRule
  , useOnRule
  , useOrRule
  , usePatternGuardsRule
  , useReplicateRule
  , useSequenceRule
  , useSpanRule
  , useTraverseRule
  , useTraverseSequenceRule
  , useUncurryRule
  , useUnlessRule
  , useUnwrapRule
  , useVoidRule
  , useWhenRule
  , useZipRule
  , whenNotRule
  ]

-- | Load config from .purelintrc file
loadConfig :: Effect _ 
loadConfig = do
  hasConfig <- FS.exists ".purelintrc"
  if hasConfig then do
    content <- FS.readTextFile ".purelintrc"
    pure $ parseConfig content
  else
    pure defaultConfig

-- | Format a source range for display
formatRange :: { start :: { line :: Int, column :: Int }, end :: { line :: Int, column :: Int } } -> String
formatRange r = show (r.start.line + 1) <> ":" <> show (r.start.column + 1) <> "-" <> show (r.end.line + 1) <> ":" <> show (r.end.column + 1)

-- | Format severity for display  
formatSeverity :: Severity -> String
formatSeverity Refactor = "[refactor]"
formatSeverity Hint = "[hint]"
formatSeverity Warning = "[warning]"
formatSeverity Error = "[error]"

-- | Format a single warning for display
formatWarning :: String -> LintWarning -> String
formatWarning filePath (LintWarning w) = 
  filePath <> ":" <> formatRange w.range <> " " <> formatSeverity w.severity <> " " <> unwrap w.ruleId <> ": " <> unwrap w.message <>
  case w.suggestion of
    Nothing -> ""
    Just (Suggestion s) -> "\n  Replace with: " <> unwrap s.replacement

-- | Lint a single file with config
lintFileWithConfig :: Array Rule -> String -> Effect Int
lintFileWithConfig rules filePath = do
  content <- FS.readTextFile filePath
  case runRules rules (SourceCode content) of
    Left err -> do
      log $ filePath <> ": " <> err
      pure 0
    Right (LintResult result) -> do
      for_ result.warnings \w -> do
        log $ formatWarning filePath w
      pure (Array.length result.warnings)

-- | Fix a single file
fixFile :: Array Rule -> String -> Effect Int
fixFile rules filePath = do
  content <- FS.readTextFile filePath
  case runRules rules (SourceCode content) of
    Left err -> do
      log $ filePath <> ": " <> err
      pure 0
    Right result@(LintResult r) -> do
      let fixedContent = applyAllFixes content result
      when (fixedContent /= content) do
        FS.writeTextFile filePath fixedContent
        log $ filePath <> ": fixed " <> show (Array.length r.warnings) <> " issue(s)"
      pure (Array.length r.warnings)

-- | Recursively find all .purs files in a directory
findPursFiles :: String -> Effect (Array String)
findPursFiles dir = do
  entries <- FS.readdir dir
  files <- Array.foldM (collectFiles dir) [] entries
  pure files
  where
  collectFiles :: String -> Array String -> String -> Effect (Array String)
  collectFiles parentDir acc entry = do
    let path = parentDir <> "/" <> entry
    -- Skip hidden directories and node_modules
    if String.take 1 entry == "." || entry == "node_modules" || entry == ".spago" then
      pure acc
    else do
      stats <- FS.stat path
      if stats.isDirectory then do
        subFiles <- findPursFiles path
        pure (acc <> subFiles)
      else if String.takeRight 5 entry == ".purs" then
        pure (Array.snoc acc path)
      else
        pure acc

-- | Lint a file or directory with config
lintPathWithConfig :: Array Rule -> String -> Effect Int
lintPathWithConfig rules path = do
  stats <- FS.stat path
  if stats.isDirectory then do
    files <- findPursFiles path
    counts <- Array.foldM (\acc file -> do
      count <- lintFileWithConfig rules file
      pure (acc + count)) 0 files
    pure counts
  else
    lintFileWithConfig rules path

-- | Fix a file or directory
fixPath :: Array Rule -> String -> Effect Int
fixPath rules path = do
  stats <- FS.stat path
  if stats.isDirectory then do
    files <- findPursFiles path
    counts <- Array.foldM (\acc file -> do
      count <- fixFile rules file
      pure (acc + count)) 0 files
    pure counts
  else
    fixFile rules path

-- | Show usage
showUsage :: Effect Unit
showUsage = do
  log "purelint - a linter for PureScript (70 rules)"
  log ""
  log "Usage: purelint [OPTIONS] [FILES/DIRECTORIES...]"
  log ""
  log "Options:"
  log "  --help     Show this help"
  log "  --fix      Auto-fix issues (where possible)"
  log "  --init     Create a sample .purelintrc"
  log "  --list     List all available rules"
  log ""
  log "Configuration:"
  log "  Create .purelintrc in your project root to disable rules."
  log "  Add one rule name per line (lines starting with # are comments)."
  log ""
  log "Examples:"
  log "  purelint src/             Lint all .purs files in src/"
  log "  purelint --fix src/       Auto-fix issues in src/"
  log "  purelint src/Main.purs    Lint a single file"

-- | List all available rules
listRules :: Effect Unit
listRules = do
  log $ "Available rules (" <> show (Array.length allRules) <> " total):"
  log ""
  for_ allRules \rule -> do
    log $ "  " <> unwrap rule.ruleId

-- | Create sample config file
initConfig :: Effect Unit
initConfig = do
  hasConfig <- FS.exists ".purelintrc"
  if hasConfig then
    log ".purelintrc already exists"
  else do
    let sampleConfig = """# purelint configuration
# Add rule names (one per line) to disable them
# Example:
# EtaReduce
# LetToWhere
"""
    FS.writeTextFile ".purelintrc" sampleConfig
    log "Created .purelintrc"

main :: Effect Unit
main = do
  args <- Process.argv
  -- args[0] is node, args[1] is the script, args[2..] are the actual arguments
  let userArgs = Array.drop 1 args
  case userArgs of
    [] -> showUsage
    ["--help"] -> showUsage
    ["-h"] -> showUsage
    ["--list"] -> listRules
    ["--init"] -> initConfig
    _ -> do
      -- Check for --fix flag
      let hasFix = Array.elem "--fix" userArgs
      let paths = Array.filter (\a -> a /= "--fix") userArgs
      
      -- Load config and filter rules
      config <- loadConfig
      let enabledRules = filterRules allRules config
      
      if hasFix then do
        -- Fix mode
        _ <- Array.foldM (\acc path -> do
          count <- fixPath enabledRules path
          pure (acc + count)) 0 paths
        pure unit
      else do
        -- Lint mode
        totalWarnings <- Array.foldM (\acc path -> do
          count <- lintPathWithConfig enabledRules path
          pure (acc + count)) 0 paths
        when (totalWarnings > 0) do
          log ""
          log $ "Total: " <> show totalWarnings <> " warning" <> if totalWarnings == 1 then "" else "s"
          Process.exit 1
