module Purelint.Tidy where

import Prelude

import Effect (Effect)

-- | Format a PureScript expression string using purs-tidy
-- | Returns the formatted string, or the original if formatting fails
foreign import formatExpr :: String -> Effect String
