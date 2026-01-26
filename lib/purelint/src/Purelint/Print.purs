module Purelint.Print where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, foldMap)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)
import Data.Either (either)
import Data.Void (Void)
import PureScript.CST.Print (printToken, printSourceToken)
import PureScript.CST.Range (tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types (Binder, Expr, SourceToken, Token(..), Type)

-- | Print an expression back to source code with tidy-style formatting
printExpr :: Expr Void -> String
printExpr expr = do
  let tokens = TokenList.toArray (tokensOf expr)
  printTokens tokens

-- | Print an expression preserving original whitespace/newlines
printExprMultiline :: Expr Void -> String
printExprMultiline expr = do
  let tokens = TokenList.toArray (tokensOf expr)
  String.trim $ foldMap printSourceToken tokens

-- | Print an expression without comments with tidy-style formatting
printExprNoComments :: Expr Void -> String
printExprNoComments = printExpr

-- | Print a binder back to source code with tidy-style formatting
printBinder :: Binder Void -> String
printBinder binder = do
  let tokens = TokenList.toArray (tokensOf binder)
  printTokens tokens

-- | Print a type back to source code with tidy-style formatting
printType :: Type Void -> String
printType typ = do
  let tokens = TokenList.toArray (tokensOf typ)
  printTokens tokens

-- | Print tokens with tidy-style spacing
printTokens :: Array SourceToken -> String
printTokens tokens = 
  String.trim $ foldl go { prev: Nothing, result: "" } tokens # _.result
  where
  go :: { prev :: Maybe Token, result :: String } -> SourceToken -> { prev :: Maybe Token, result :: String }
  go acc tok = 
    let 
      t = tok.value
      printed = printToken t
      space = needsSpaceBetween acc.prev t
    in
      { prev: Just t
      , result: acc.result <> space <> printed
      }

-- | Determine spacing between two tokens (tidy-style)
needsSpaceBetween :: Maybe Token -> Token -> String
needsSpaceBetween prev curr = case prev of
  Nothing -> ""
  Just p -> 
    -- Space after opening brackets: [ x, { a:
    if needsSpaceAfter p then " "
    -- Space before closing brackets: x ], a }
    else if needsSpaceBefore curr then " "
    -- No space after certain tokens
    else if noSpaceAfter p then ""
    -- No space before certain tokens
    else if noSpaceBefore curr then ""
    -- Default: space between tokens
    else " "

-- | Tokens that need space after them (tidy-style)
needsSpaceAfter :: Token -> Boolean
needsSpaceAfter = case _ of
  TokLeftSquare -> true   -- [ x
  TokLeftBrace -> true    -- { a
  _ -> false

-- | Tokens that need space before them (tidy-style)
needsSpaceBefore :: Token -> Boolean
needsSpaceBefore = case _ of
  TokRightSquare -> true  -- x ]
  TokRightBrace -> true   -- a }
  _ -> false

-- | Tokens that should NOT have space after them
noSpaceAfter :: Token -> Boolean
noSpaceAfter = case _ of
  TokBackslash -> true    -- \x
  TokLeftParen -> true    -- (x
  TokDot -> true          -- a.b
  TokAt -> true           -- @x
  TokPipe -> true         -- |x in guards
  _ -> false

-- | Tokens that should NOT have space before them
noSpaceBefore :: Token -> Boolean
noSpaceBefore = case _ of
  TokRightParen -> true   -- x)
  TokComma -> true        -- x,
  TokDot -> true          -- a.b
  _ -> false

-- | Collapse multiple spaces into single space and trim
normalizeWhitespace :: String -> String
normalizeWhitespace s = do
  let
    collapsed = either (const s) (\r -> replace r " " s) (regex "\\s+" global)
  String.trim collapsed
