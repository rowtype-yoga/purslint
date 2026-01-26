module Purelint.Config where

import Prelude

import Data.Array (notElem)
import Data.Array as Array
import Data.String as String
import Purelint.Types (RuleId(..))

-- | Parsed config - just enabled/disabled rules for now
type Config =
  { disabledRules :: Array RuleId
  }

defaultConfig :: Config
defaultConfig =
  { disabledRules: []
  }

-- | Parse simple line-based config
parseConfig :: String -> Config
parseConfig str =
  { disabledRules: rules }
  where
  lines = str # String.split (String.Pattern "\n")
  nonEmpty = lines # Array.filter \l -> String.length l > 0 && not (isComment l)
  rules = (RuleId <<< String.trim) <$> nonEmpty

  isComment :: String -> Boolean
  isComment line = (trimmed # String.take 1) == "#"
    where
    trimmed = String.trim line

-- | Check if a rule is enabled given the config
isRuleEnabled :: Config -> RuleId -> Boolean
isRuleEnabled config ruleId = notElem ruleId config.disabledRules

-- | Filter rules by config
filterRules :: forall a. Array { ruleId :: RuleId | a } -> Config -> Array { ruleId :: RuleId | a }
filterRules rules config = rules # Array.filter \r -> r.ruleId # isRuleEnabled config
