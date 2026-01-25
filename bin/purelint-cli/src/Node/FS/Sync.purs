module Node.FS.Sync where

import Prelude
import Effect (Effect)

-- | Read file contents as a string
foreign import readTextFile :: String -> Effect String

-- | Write file contents as a string
foreign import writeTextFile :: String -> String -> Effect Unit

-- | Check if file exists
foreign import exists :: String -> Effect Boolean

-- | Read directory contents
foreign import readdir :: String -> Effect (Array String)

-- | Get file stats
type Stats =
  { isFile :: Boolean
  , isDirectory :: Boolean
  , isSymbolicLink :: Boolean
  , size :: Int
  }

foreign import stat :: String -> Effect Stats
