module Purelint.ImportFix where

import Prelude

import Data.Maybe (Maybe)
import PureScript.CST.Types (SourceToken)

-- [TODO]: This got lost, I'm not sure what it should do
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
processImports source replacement imports = do
  { linesAdded: 0, replacement: "", source: source }