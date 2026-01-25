module Purelint.Rules.NothingBind where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Void (Void)
import Purelint.Imports (ImportInfo, hasOp)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Expr(..), Ident(..), Module, Operator(..), Proper(..), QualifiedName(..), Wrapped(..))

-- | Rule: Nothing >>= f -> Nothing
-- | Also: f =<< Nothing -> Nothing
nothingBindRule :: Rule
nothingBindRule = mkRule (RuleId "NothingBind") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: Nothing >>= f
    ExprOp lhs ops
      | hasOp imports ">>=" ->
        case NEA.toArray ops of
          [Tuple (QualifiedName { name: Operator ">>=" }) _rhs] ->
            if isNothing (unwrapParens lhs) then
              [ LintWarning
                  { ruleId: RuleId "NothingBind"
                  , message: WarningMessage "Nothing >>= f is always Nothing"
                  , range: rangeOf expr
                  , severity: Warning
                  , suggestion: Just $ Suggestion
                      { replacement: ReplacementText "Nothing"
                      , description: SuggestionDescription "Simplify Nothing >>= f to Nothing"
                      }
                  }
              ]
            else []
          [Tuple (QualifiedName { name: Operator "=<<" }) rhs] ->
            if isNothing (unwrapParens rhs) then
              [ LintWarning
                  { ruleId: RuleId "NothingBind"
                  , message: WarningMessage "f =<< Nothing is always Nothing"
                  , range: rangeOf expr
                  , severity: Warning
                  , suggestion: Just $ Suggestion
                      { replacement: ReplacementText "Nothing"
                      , description: SuggestionDescription "Simplify f =<< Nothing to Nothing"
                      }
                  }
              ]
            else []
          _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isNothing :: Expr Void -> Boolean
  isNothing (ExprIdent (QualifiedName { name: Ident "Nothing" })) = true
  isNothing (ExprConstructor (QualifiedName { name: Proper "Nothing" })) = true
  isNothing _ = false
