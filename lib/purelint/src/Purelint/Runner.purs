module Purelint.Runner where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Purelint.Imports (getImportInfo)
import Purelint.Rule (Rule, RuleContext)
import Purelint.Types (LintResult(..), SourceCode(..))
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Types (Module, ModuleHeader(..), ModuleName)
import Unsafe.Coerce (unsafeCoerce)

-- | Run a set of rules on source code
runRules :: Array Rule -> SourceCode -> Either String LintResult
runRules rules (SourceCode source) = 
  case parseModule source of
    ParseSucceeded m -> Right $ runOnModule rules (toVoidModule m)
    ParseSucceededWithErrors m _ -> Right $ runOnModule rules (toVoidModule m)
    ParseFailed errRec -> Left $ "Parse error at " <> show (errRec.position.line + 1) <> ":" <> show (errRec.position.column + 1) <> ": " <> printParseError errRec.error

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
