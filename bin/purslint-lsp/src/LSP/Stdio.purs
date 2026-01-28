module LSP.Stdio where

import Prelude
import Data.Maybe (Maybe)
import Effect (Effect)
import Foreign (Foreign)

-- | Read a JSON-RPC message from stdin
foreign import readMessage :: Effect (Maybe String)

-- | Write a JSON-RPC message to stdout
foreign import writeMessage :: String -> Effect Unit

-- | Log a message to stderr (for debugging)
foreign import logMessage :: String -> Effect Unit

-- | Parse JSON string to Foreign
foreign import parseJSON :: String -> Maybe Foreign

-- | Stringify Foreign to JSON
foreign import stringifyJSON :: Foreign -> String
