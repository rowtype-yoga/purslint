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
import Node.FS.Sync as FS
import Node.Process as Process
import Purelint.Config (defaultConfig, filterRules, parseConfig)
import Purelint.Fix (applyAllFixes)
import Purelint.Rule (Rule)
import Purelint.Rules.ConcatMap (concatMapRule)
import Purelint.Rules.EtaReduce (etaReduceRule)
import Purelint.Rules.EtaReduceDecl (etaReduceDeclRule)
import Purelint.Rules.FmapId (fmapIdRule)
import Purelint.Rules.LetToWhere (letToWhereRule)
import Purelint.Rules.MapFusion (mapFusionRule)
import Purelint.Rules.NotEqual (notEqualRule)
import Purelint.Rules.RedundantBind (redundantBindRule)
import Purelint.Rules.UseGuard (useGuardRule)
import Purelint.Rules.UseTraverse (useTraverseRule)
import Purelint.Runner (runRules)
import Purelint.Types (LintResult(..), LintWarning(..), Severity(..), SourceCode(..), Suggestion(..))

-- | All available rules
allRules :: Array Rule
allRules =
  [ useTraverseRule
  , mapFusionRule
  , fmapIdRule
  , notEqualRule
  , concatMapRule
  , useGuardRule
  , etaReduceRule
  , etaReduceDeclRule
  , redundantBindRule
  , letToWhereRule
  ]

-- | Load config from .purelintignore file
loadConfig :: Effect _ 
loadConfig = do
  hasConfig <- FS.exists ".purelintignore"
  if hasConfig then do
    content <- FS.readTextFile ".purelintignore"
    pure $ parseConfig content
  else
    pure defaultConfig

-- | Format a source range for display
formatRange :: { start :: { line :: Int, column :: Int }, end :: { line :: Int, column :: Int } } -> String
formatRange r = show (r.start.line + 1) <> ":" <> show (r.start.column + 1) <> "-" <> show (r.end.line + 1) <> ":" <> show (r.end.column + 1)

-- | Format severity for display  
formatSeverity :: Severity -> String
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
  log "purelint - a linter for PureScript"
  log ""
  log "Usage: purelint [OPTIONS] [FILES/DIRECTORIES...]"
  log ""
  log "Options:"
  log "  --help     Show this help"
  log "  --fix      Auto-fix issues (where possible)"
  log "  --init     Create a sample .purelintignore"
  log "  --list     List all available rules"
  log ""
  log "Configuration:"
  log "  Create .purelintignore in your project root to disable rules."
  log "  Add one rule name per line (lines starting with # are comments)."
  log ""
  log "Examples:"
  log "  purelint src/             Lint all .purs files in src/"
  log "  purelint --fix src/       Auto-fix issues in src/"
  log "  purelint src/Main.purs    Lint a single file"

-- | List all available rules
listRules :: Effect Unit
listRules = do
  log "Available rules:"
  log ""
  for_ allRules \rule -> do
    log $ "  " <> unwrap rule.ruleId

-- | Create sample config file
initConfig :: Effect Unit
initConfig = do
  hasConfig <- FS.exists ".purelintignore"
  if hasConfig then
    log ".purelintignore already exists"
  else do
    let sampleConfig = """# purelint ignore file
# Add rule names (one per line) to disable them
# Example:
# EtaReduce
# MapFusion
"""
    FS.writeTextFile ".purelintignore" sampleConfig
    log "Created .purelintignore"

main :: Effect Unit
main = do
  args <- Process.argv
  -- args[0] is the executable, args[1..] are the actual arguments
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
