module Purelint.ImportFix where

import Data.Maybe (Maybe)
-- NOTE: Import processing is currently a no-op. We assume required imports
-- are already present in the source and keep replacements intact.
processImports :: String -> String -> Array
            { codeText :: Maybe String
            , importItem :: Maybe String
            , moduleName :: String
            , qualifier :: Maybe String
            }
          -> { linesAdded :: Int
             , replacement :: String
             , source :: String
             }
processImports source replacement _imports =
  { linesAdded: 0, replacement, source }