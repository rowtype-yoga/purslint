module Purslint.Rules.UseHead where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Purslint.Imports (ImportInfo, hasValue)
import Purslint.Rule (Rule, RuleContext, mkRule)
import Purslint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Expr(..), Ident(..), IntValue(..), Module, Operator(..), QualifiedName(..), Wrapped(..))
import Data.Array.NonEmpty as NEA
import Data.Tuple (Tuple(..))

-- | Rule: xs !! 0 -> head xs
useHeadRule :: Rule
useHeadRule = mkRule (RuleId "UseHead") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: xs !! 0
    ExprOp lhs ops
      | hasValue imports "head" ->
        case NEA.toArray ops of
          [Tuple (QualifiedName { name: Operator "!!" }) rhs] ->
            if isZeroLiteral (unwrapParens rhs) then
              [ LintWarning
                  { ruleId: RuleId "UseHead"
                  , message: WarningMessage "xs !! 0 can be simplified to head xs"
                  , range: rangeOf expr
                  , severity: Hint
                  , suggestion: Just $ Suggestion
                      { replacement: ReplacementText "head"
                      , description: SuggestionDescription "Use head instead of !! 0"
                        , requiredImports: []
                      }
                  }
              ]
            else []
          _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isZeroLiteral :: Expr Void -> Boolean
  isZeroLiteral (ExprInt _ (SmallInt 0)) = true
  isZeroLiteral (ExprInt _ (BigInt s)) = s == "0"
  isZeroLiteral _ = false

