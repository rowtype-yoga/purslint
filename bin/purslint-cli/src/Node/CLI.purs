module Node.CLI where

import Prelude
import Effect (Effect)

-- | Get command line arguments
foreign import argv :: Effect (Array String)

-- | Get current working directory
foreign import cwd :: Effect String

-- | Exit with status code
foreign import exit :: Int -> Effect Unit
