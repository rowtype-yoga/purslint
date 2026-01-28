module Purslint.Rule where

import Prelude

import Purslint.Imports (ImportInfo)
import Purslint.Types (LintWarning, RuleId)
import PureScript.CST.Types (Module)
import Data.Void (Void)

-- | Context passed to each rule containing import information
type RuleContext =
  { imports :: ImportInfo
  }

-- | A lint rule that can analyze a CST module and produce warnings
type Rule =
  { ruleId :: RuleId
  , run :: RuleContext -> Module Void -> Array LintWarning
  }

-- | Create a rule from an id and a function
mkRule :: RuleId -> (RuleContext -> Module Void -> Array LintWarning) -> Rule
mkRule ruleId run = { ruleId, run }
