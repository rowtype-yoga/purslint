module Purelint.Print where

import Prelude

import Data.Foldable (foldMap)
import Data.String as String
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)
import Data.Either (either)
import Data.Void (Void)
import PureScript.CST.Print (printSourceToken)
import PureScript.CST.Range (class TokensOf, tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types (Expr)

-- | Print an expression back to source code, normalizing whitespace
printExpr :: Expr Void -> String
printExpr expr = 
  let raw = foldMap printSourceToken (TokenList.toArray (tokensOf expr))
  in normalizeWhitespace raw

-- | Collapse multiple spaces into single space and trim
normalizeWhitespace :: String -> String
normalizeWhitespace s = 
  let 
    -- Replace multiple spaces/newlines with single space
    collapsed = either (const s) (\r -> replace r " " s) (regex "\\s+" global)
  in String.trim collapsed
