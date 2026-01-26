module Purelint.Runner where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Newtype (un, unwrap)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Types (ImportDecl(..), Module(..), ModuleHeader(..), ModuleName(..), Name(..))
import Purelint.Imports (getImportInfo, ImportInfo)
import Purelint.Rule (Rule, RuleContext)
import Purelint.Types (LintResult(..), SourceCode(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Run a set of rules on source code
runRules :: Array Rule -> SourceCode -> Either String LintResult
runRules rules (SourceCode source) =
  case parseModule source of
    ParseSucceeded m -> Right $ runOnModule rules (toVoidModule m)
    ParseSucceededWithErrors m _ -> Right $ runOnModule rules (toVoidModule m)
    ParseFailed errRec -> Left $ "Parse error at " <> show (errRec.position.line + 1) <> ":" <> show (errRec.position.column + 1) <> ": " <> printParseError errRec.error

-- | Get import info from source code (for debugging)
getImportsFromSource :: SourceCode -> Either String ImportInfo
getImportsFromSource (SourceCode source) =
  case parseModule source of
    ParseSucceeded m -> Right $ getImportInfo (toVoidModule m)
    ParseSucceededWithErrors m _ -> Right $ getImportInfo (toVoidModule m)
    ParseFailed _ -> Left "Parse error"

-- | Get raw module names from imports (for debugging)
getModuleNames :: SourceCode -> Either String (Array String)
getModuleNames (SourceCode source) =
  case parseModule source of
    ParseSucceeded m -> Right $ extractModuleNames (toVoidModule m)
    ParseSucceededWithErrors m _ -> Right $ extractModuleNames (toVoidModule m)
    ParseFailed _ -> Left "Parse error"
  where
  extractModuleNames (Module mod) = do
    let ModuleHeader header = mod.header
    header.imports <#> extractName
  extractName (ImportDecl imp) =
    case imp.module of
      Name { name: ModuleName n } -> n

-- | Run rules on a successfully parsed module
runOnModule :: Array Rule -> Module Void -> LintResult
runOnModule rules mod =
  LintResult { warnings, moduleName: modName }
  where
  imports = getImportInfo mod

  ctx :: RuleContext
  ctx = { imports }
  warnings = rules # Array.concatMap \rule -> rule.run ctx mod
  modName = getModuleName mod

-- | Get the module name from a parsed module
getModuleName :: forall e. Module e -> ModuleName
getModuleName mod =
  (header.name # un Name).name
  where
  ModuleHeader header = (mod # un Module).header

-- | Convert a recovered module to a Void module (assuming no errors)
-- | This is safe after ParseSucceeded because there are no error nodes
toVoidModule :: forall e. Module e -> Module Void
toVoidModule = unsafeCoerce
