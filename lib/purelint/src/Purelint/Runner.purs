module Purelint.Runner where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Purelint.Imports (getImportInfo, ImportInfo)
import Purelint.Rule (Rule, RuleContext)
import Purelint.Types (LintResult(..), SourceCode(..))
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Types (Module, ModuleHeader(..), ModuleName(..), ImportDecl(..), Name(..))
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
  extractModuleNames mod =
    let (ModuleHeader header) = (unwrap mod).header
    in map extractName header.imports
  extractName (ImportDecl imp) = 
    case imp.module of
      Name { name: ModuleName n } -> n

-- | Run rules on a successfully parsed module
runOnModule :: Array Rule -> Module Void -> LintResult
runOnModule rules mod = 
  let 
    imports = getImportInfo mod
    ctx :: RuleContext
    ctx = { imports }
    warnings = Array.concatMap (\rule -> rule.run ctx mod) rules
    modName = getModuleName mod
  in
    LintResult { warnings, moduleName: modName }

-- | Get the module name from a parsed module
getModuleName :: forall e. Module e -> ModuleName
getModuleName mod = 
  let (ModuleHeader header) = (unwrap mod).header
  in (unwrap header.name).name

-- | Convert a recovered module to a Void module (assuming no errors)
-- | This is safe after ParseSucceeded because there are no error nodes
toVoidModule :: forall e. Module e -> Module Void
toVoidModule = unsafeCoerce
