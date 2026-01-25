module Purelint.Types where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import PureScript.CST.Types as CST

-- | A rule identifier like "UseTraverse" or "EtaReduce"
newtype RuleId = RuleId String

derive instance Newtype RuleId _
derive newtype instance Eq RuleId
derive newtype instance Ord RuleId
derive newtype instance Show RuleId

-- | Severity of a lint warning
data Severity
  = Hint
  | Warning
  | Error

derive instance Eq Severity
derive instance Ord Severity

instance Show Severity where
  show Hint = "hint"
  show Warning = "warning"
  show Error = "error"

-- | A suggested replacement for the original code
newtype Suggestion = Suggestion
  { replacement :: ReplacementText
  , description :: SuggestionDescription
  }

derive instance Newtype Suggestion _

-- | The text to replace the original code with
newtype ReplacementText = ReplacementText String

derive instance Newtype ReplacementText _
derive newtype instance Eq ReplacementText
derive newtype instance Show ReplacementText

-- | Human-readable description of what the suggestion does
newtype SuggestionDescription = SuggestionDescription String

derive instance Newtype SuggestionDescription _
derive newtype instance Eq SuggestionDescription
derive newtype instance Show SuggestionDescription

-- | A message explaining why the lint rule triggered
newtype WarningMessage = WarningMessage String

derive instance Newtype WarningMessage _
derive newtype instance Eq WarningMessage
derive newtype instance Show WarningMessage

-- | A lint warning produced by a rule
newtype LintWarning = LintWarning
  { ruleId :: RuleId
  , message :: WarningMessage
  , range :: CST.SourceRange
  , severity :: Severity
  , suggestion :: Maybe Suggestion
  }

derive instance Newtype LintWarning _

-- | Result of running all lint rules on a module
newtype LintResult = LintResult
  { warnings :: Array LintWarning
  , moduleName :: CST.ModuleName
  }

derive instance Newtype LintResult _

-- | A file path to lint
newtype FilePath = FilePath String

derive instance Newtype FilePath _
derive newtype instance Eq FilePath
derive newtype instance Ord FilePath
derive newtype instance Show FilePath

-- | Source code content
newtype SourceCode = SourceCode String

derive instance Newtype SourceCode _
derive newtype instance Eq SourceCode
derive newtype instance Show SourceCode
