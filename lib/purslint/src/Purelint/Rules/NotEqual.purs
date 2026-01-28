module Purelint.Rules.NotEqual where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Purelint.Imports (ImportInfo, hasValue, hasOp)
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, Operator(..), QualifiedName(..), Wrapped(..))
import Data.Void (Void)

-- | Rule: not (a == b) -> a /= b
-- | Also: not (a /= b) -> a == b
notEqualRule :: Rule
notEqualRule = mkRule (RuleId "NotEqual") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: not (a == b) or not (a /= b)
    ExprApp fnExpr args ->
      case fnExpr of
        ExprIdent qn | isNot imports qn ->
          case NEA.head args of
            AppTerm innerExpr -> checkNotComparison imports expr (unwrapParens innerExpr)
            _ -> []
        _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  checkNotComparison :: ImportInfo -> Expr Void -> Expr Void -> Array LintWarning
  checkNotComparison imports fullExpr innerExpr = 
    case innerExpr of
      -- Match: a == b or a /= b
      ExprOp lhs ops | Just opInfo <- getComparisonOp imports ops ->
        let 
          (Tuple opName suggested) = opInfo
          lhsText = printExpr lhs
          -- Get RHS from the ops
          rhsText = case NEA.toArray ops of
            [Tuple _ rhs] -> printExpr rhs
            _ -> "b"
          replacement = lhsText <> " " <> suggested <> " " <> rhsText
          msg = "not (a " <> opName <> " b) can be simplified to a " <> suggested <> " b"
        in
          [ LintWarning
              { ruleId: RuleId "NotEqual"
              , message: WarningMessage $ "Use " <> suggested <> " instead of not ... " <> opName
              , range: rangeOf fullExpr
              , severity: Warning
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText replacement
                  , description: SuggestionDescription msg
                    , requiredImports: []
                  }
              }
          ]
      _ -> []

  getComparisonOp :: ImportInfo -> _ -> Maybe (Tuple String String)
  getComparisonOp imports ops = 
    let arr = NEA.toArray ops
    in case arr of
      [Tuple (QualifiedName { name: Operator op }) _] 
        | op == "==" && hasOp imports "==" -> Just (Tuple "==" "/=")
        | op == "/=" && hasOp imports "/=" -> Just (Tuple "/=" "==")
      _ -> Nothing

  isNot :: ImportInfo -> QualifiedName Ident -> Boolean
  isNot imports (QualifiedName { name: Ident name }) = name == "not" && hasValue imports "not"

