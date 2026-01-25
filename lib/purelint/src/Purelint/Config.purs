module Purelint.Config where

import Prelude

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
  let 
    lines = String.split (String.Pattern "\n") str
    nonEmpty = Array.filter (\l -> String.length l > 0 && not (isComment l)) lines
    rules = map (RuleId <<< String.trim) nonEmpty
  in
    { disabledRules: rules }

  where
  isComment :: String -> Boolean
  isComment line = 
    let trimmed = String.trim line
    in String.take 1 trimmed == "#"

-- | Check if a rule is enabled given the config
isRuleEnabled :: Config -> RuleId -> Boolean
isRuleEnabled config ruleId = not (Array.elem ruleId config.disabledRules)

-- | Filter rules by config
filterRules :: forall a. Array { ruleId :: RuleId | a } -> Config -> Array { ruleId :: RuleId | a }
filterRules rules config = Array.filter (\r -> isRuleEnabled config r.ruleId) rules
