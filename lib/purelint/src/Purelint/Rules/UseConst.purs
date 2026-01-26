module Purelint.Rules.UseConst where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.String as String
import Data.Void (Void)
import Purelint.Imports (ImportInfo, hasValue)
import Purelint.Print (printExpr, printExprMultiline)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Binder(..), Expr(..), Module)

-- | Rule: \_ -> y -> const y
-- | Only triggers when the binder is a simple unused variable
useConstRule :: Rule
useConstRule = mkRule (RuleId "UseConst") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    ExprLambda lambda | hasValue imports "const" ->
      case NEA.toArray lambda.binders of
        -- Only match wildcard binder: \_ -> body
        -- Named unused params like \f -> body are already warned by the compiler
        [BinderWildcard _] ->
            let
              bodyRange = rangeOf lambda.body
              isMultiline = bodyRange.start.line /= bodyRange.end.line
              replacement = 
                if isMultiline then
                  -- Preserve multiline structure: const\n<original body>
                  let
                    bodyStr = printExprMultiline lambda.body
                    -- Get the column where the body starts for indent reference
                    bodyCol = bodyRange.start.column
                    indent n = power " " n
                    -- The first line has no indentation (due to trim), subsequent lines have original
                    lines = String.split (String.Pattern "\n") bodyStr
                    -- Find minimum non-empty line indent from lines AFTER the first
                    minIndent = case Array.tail lines of
                      Nothing -> 0
                      Just rest -> rest
                        # Array.filter (\l -> String.trim l /= "")
                        # map getIndent
                        # Array.foldl min 999
                    -- Re-indent: first line gets bodyCol indent, rest get normalized
                    reindented = case Array.uncons lines of
                      Nothing -> ""
                      Just { head: first, tail: rest } ->
                        let 
                          firstReindented = indent bodyCol <> first
                          restReindented = rest # map (\l -> 
                            if String.trim l == "" then "" 
                            else indent bodyCol <> String.drop minIndent l
                          )
                        in String.joinWith "\n" (Array.cons firstReindented restReindented)
                  in
                    "const\n" <> reindented
                else
                  "const " <> printExpr lambda.body
            in
              [ LintWarning
                  { ruleId: RuleId "UseConst"
                  , message: WarningMessage "Lambda ignores its argument"
                  , range: rangeOf expr
                  , severity: Hint
                  , suggestion: Just $ Suggestion
                      { replacement: ReplacementText replacement
                      , description: SuggestionDescription "\\_ -> y can be simplified to const y"
                      }
                  }
              ]
        _ -> []
    _ -> []

  -- Get the number of leading spaces in a string
  getIndent :: String -> Int
  getIndent s = 
    let loop i = if String.take 1 (String.drop i s) == " " then loop (i + 1) else i
    in loop 0
