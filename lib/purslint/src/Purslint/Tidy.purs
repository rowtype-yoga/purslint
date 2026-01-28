module Purslint.Tidy where

import Prelude

import Effect (Effect)

-- | Format a full PureScript module string using purs-tidy
-- | Returns the formatted string, or the original if formatting fails
foreign import formatModule :: String -> Effect String

-- | Format a PureScript expression string using purs-tidy
-- | Returns the formatted string, or the original if formatting fails
foreign import formatExpr :: String -> Effect String
