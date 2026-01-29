module Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String.CodeUnits as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Simple as FS
import Node.CLI as Process
import Options.Applicative (Parser, argument, execParserPure, fullDesc, header, help, helper, info, long, many, metavar, progDesc, str, switch, (<**>))
import Options.Applicative.Builder (defaultPrefs)
import Options.Applicative.Types (CompletionResult(..), ParserFailure(ParserFailure), ParserResult(..))
import Options.Applicative.Help.Types (ParserHelp(ParserHelp), renderHelp)
import Purslint.Config (defaultConfig, filterRules, parseConfig)
import Purslint.Fix (applyAllFixes)
import Purslint.Rule (Rule)
import Purslint.Rules.BooleanSimplify (booleanSimplifyRule)
import Purslint.Rules.CollapseLambdas (collapseLambdasRule)
import Purslint.Rules.ConcatMap (concatMapRule)
import Purslint.Rules.EtaReduce (etaReduceRule)
import Purslint.Rules.EtaReduceDecl (etaReduceDeclRule)
import Purslint.Rules.AlternativeLaw (alternativeLawRule)
import Purslint.Rules.EvaluateBool (evaluateBoolRule)
import Purslint.Rules.EvaluateConst (evaluateConstRule)
import Purslint.Rules.EvaluateEither (evaluateEitherRule)
import Purslint.Rules.EvaluateFst (evaluateFstRule)
import Purslint.Rules.MapIdentity (mapIdentityRule)
import Purslint.Rules.FunctorLaw (functorLawRule)
import Purslint.Rules.LetToDo (letToDoRule)
import Purslint.Rules.LetToWhere (letToWhereRule)
import Purslint.Rules.MapFusion (mapFusionRule)
import Purslint.Rules.MonadLaw (monadLawRule)
import Purslint.Rules.MonoidIdentity (monoidIdentityRule)
import Purslint.Rules.NotEqual (notEqualRule)
import Purslint.Rules.NothingBind (nothingBindRule)
import Purslint.Rules.RedundantBind (redundantBindRule)
import Purslint.Rules.RedundantFlip (redundantFlipRule)
import Purslint.Rules.RedundantGuard (redundantGuardRule)
import Purslint.Rules.RedundantId (redundantIdRule)
import Purslint.Rules.RedundantIf (redundantIfRule)
import Purslint.Rules.RedundantNegate (redundantNegateRule)
import Purslint.Rules.RedundantNot (redundantNotRule)
import Purslint.Rules.RedundantReverse (redundantReverseRule)
import Purslint.Rules.UnlessNot (unlessNotRule)
import Purslint.Rules.UseAll (useAllRule)
import Purslint.Rules.UseAnd (useAndRule)
import Purslint.Rules.UseAny (useAnyRule)
import Purslint.Rules.UseApplicative (useApplicativeRule)
import Purslint.Rules.UseApplyFlipped (useApplyFlippedRule)
import Purslint.Rules.UseBimap (useBimapRule)
import Purslint.Rules.UseBindFlip (useBindFlipRule)
import Purslint.Rules.UseBreak (useBreakRule)
import Purslint.Rules.UseComparing (useComparingRule)
import Purslint.Rules.UseConst (useConstRule)
import Purslint.Rules.UseDollar (useDollarRule)
import Purslint.Rules.UseEitherMap (useEitherMapRule)
import Purslint.Rules.UseElemIndex (useElemIndexRule)
import Purslint.Rules.UseFindMap (useFindMapRule)
import Purslint.Rules.UseFold (useFoldRule)
import Purslint.Rules.UseFoldBool (useFoldBoolRule)
import Purslint.Rules.UseFoldMap (useFoldMapRule)
import Purslint.Rules.UseFoldMapId (useFoldMapIdRule)
import Purslint.Rules.UseFor (useForRule)
import Purslint.Rules.UseFromJust (useFromJustRule)
import Purslint.Rules.UseFromMaybe (useFromMaybeRule)
import Purslint.Rules.UseFstSnd (useFstSndRule)
import Purslint.Rules.UseGuard (useGuardRule)
import Purslint.Rules.UseGuardMaybe (useGuardMaybeRule)
import Purslint.Rules.UseHead (useHeadRule)
import Purslint.Rules.UseIsJust (useIsJustRule)
import Purslint.Rules.UseIsNothing (useIsNothingRule)
import Purslint.Rules.UseJoin (useJoinRule)
import Purslint.Rules.UseLastReverse (useLastReverseRule)
import Purslint.Rules.UseMapMaybe (useMapMaybeRule)
import Purslint.Rules.UseMaybeMap (useMaybeMapRule)
import Purslint.Rules.UseMinMax (useMinMaxRule)
import Purslint.Rules.UseMinimumSort (useMinimumSortRule)
import Purslint.Rules.UseNotElem (useNotElemRule)
import Purslint.Rules.UseNull (useNullRule)
import Purslint.Rules.UseCaseOf (useCaseOfRule)
import Purslint.Rules.FlattenCase (flattenCaseRule)
import Purslint.Rules.UseOn (useOnRule)
import Purslint.Rules.UseOr (useOrRule)
import Purslint.Rules.UsePatternGuards (usePatternGuardsRule)
import Purslint.Rules.UseReplicate (useReplicateRule)
import Purslint.Rules.UseSequence (useSequenceRule)
import Purslint.Rules.UseSpan (useSpanRule)
import Purslint.Rules.UseTraverse (useTraverseRule)
import Purslint.Rules.UseTraverseSequence (useTraverseSequenceRule)
import Purslint.Rules.UseUncurry (useUncurryRule)
import Purslint.Rules.UseUnless (useUnlessRule)
import Purslint.Rules.UseUnwrap (useUnwrapRule)
import Purslint.Rules.UseVoid (useVoidRule)
import Purslint.Rules.UseWhen (useWhenRule)
import Purslint.Rules.UseZip (useZipRule)
import Purslint.Rules.WhenNot (whenNotRule)
import Purslint.Rules.RedundantParens (redundantParensRule)
import Purslint.Runner (runRules)
import Purslint.Types (LintResult(..), LintWarning(..), Severity(..), SourceCode(..), Suggestion(..))

type CliOptions =
  { fix :: Boolean
  , listRules :: Boolean
  , initConfig :: Boolean
  , paths :: Array String
  }

-- | All available rules (81 total)
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
  , redundantParensRule
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
  , useDollarRule
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
  , useCaseOfRule
  , flattenCaseRule
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

-- | Load config from .purslintrc file
loadConfig :: Effect _ 
loadConfig = do
  hasConfig <- FS.exists ".purslintrc"
  if hasConfig then do
    content <- FS.readTextFile ".purslintrc"
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
  log "purslint - a linter for PureScript (81 rules)"
  log ""
  log "Usage: purslint [OPTIONS] [FILES/DIRECTORIES...]"
  log ""
  log "Options:"
  log "  --help     Show this help"
  log "  --fix      Auto-fix issues (where possible)"
  log "  --init     Create a sample .purslintrc"
  log "  --list     List all available rules"
  log ""
  log "Configuration:"
  log "  Create .purslintrc in your project root to disable rules."
  log "  Add one rule name per line (lines starting with # are comments)."
  log ""
  log "Examples:"
  log "  purslint src/             Lint all .purs files in src/"
  log "  purslint --fix src/       Auto-fix issues in src/"
  log "  purslint src/Main.purs    Lint a single file"

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
  hasConfig <- FS.exists ".purslintrc"
  if hasConfig then
    log ".purslintrc already exists"
  else do
    let sampleConfig = """# purslint configuration
# Add rule names (one per line) to disable them
# Example:
# EtaReduce
# LetToWhere
"""
    FS.writeTextFile ".purslintrc" sampleConfig
    log "Created .purslintrc"

parseFlags :: Array String -> Effect (Maybe CliOptions)
parseFlags args = case parseFlags' (Array.drop 2 args) of
  Success cli -> pure (Just cli)
  Failure (ParserFailure pf) -> case pf programName of
    ph@(ParserHelp _) /\ exitCode /\ i /\ _unit -> do
      log $ renderHelp i ph
      Process.exit (fromEnum exitCode)
      pure Nothing
  CompletionInvoked (CompletionResult { execCompletion }) -> do
    executedCompletion <- execCompletion programName
    log executedCompletion
    Process.exit 0
    pure Nothing
  where
  programName = "purslint"

parseFlags' :: Array String -> ParserResult CliOptions
parseFlags' = execParserPure defaultPrefs opts
  where
  opts =
    info (flagsParser <**> helper) $
      fullDesc
        <> progDesc "Lint PureScript source files"
        <> header "purslint - a linter for PureScript"

flagsParser :: Parser CliOptions
flagsParser = ado
  fix <- switch $ long "fix" <> help "Auto-fix issues (where possible)"
  listRules <- switch $ long "list" <> help "List all available rules"
  initConfig <- switch $ long "init" <> help "Create a sample .purslintrc"
  pathsList <- many $ argument str $ metavar "PATH..."
  let paths = Array.fromFoldable pathsList
  in { fix, listRules, initConfig, paths }

main :: Effect Unit
main = do
  args <- Process.argv
  maybeCli <- parseFlags args
  case maybeCli of
    Nothing -> pure unit
    Just cli ->
      if cli.listRules then
        listRules
      else if cli.initConfig then
        initConfig
      else if Array.null cli.paths then
        showUsage
      else do
        -- Load config and filter rules
        config <- loadConfig
        let enabledRules = filterRules allRules config

        if cli.fix then do
          -- Fix mode
          _ <- Array.foldM (\acc path -> do
            count <- fixPath enabledRules path
            pure (acc + count)) 0 cli.paths
          pure unit
        else do
          -- Lint mode
          totalWarnings <- Array.foldM (\acc path -> do
            count <- lintPathWithConfig enabledRules path
            pure (acc + count)) 0 cli.paths
          when (totalWarnings > 0) do
            log ""
            log $ "Total: " <> show totalWarnings <> " warning" <> if totalWarnings == 1 then "" else "s"
            Process.exit 1
