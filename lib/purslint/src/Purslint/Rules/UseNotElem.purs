module Purslint.Rules.UseNotElem where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Purslint.Imports (ImportInfo, hasValue)
import Purslint.Print (printExpr)
import Purslint.Rule (Rule, RuleContext, mkRule)
import Purslint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, QualifiedName(..), Wrapped(..))

-- | Rule: not (elem x y) -> notElem x y
useNotElemRule :: Rule
useNotElemRule = mkRule (RuleId "UseNotElem") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: not (elem x y)
    ExprApp fnExpr args | isNot imports fnExpr ->
      case NEA.toArray args of
        [AppTerm innerExpr] ->
          case getElemArgs imports innerExpr of
            Just { x, y } ->
              let xText = printExpr x
                  yText = printExpr y
              in
                [ LintWarning
                    { ruleId: RuleId "UseNotElem"
                    , message: WarningMessage "not (elem x y) can be simplified to notElem x y"
                    , range: rangeOf expr
                    , severity: Warning
                    , suggestion: Just $ Suggestion
                        { replacement: ReplacementText ("notElem " <> xText <> " " <> yText)
                        , description: SuggestionDescription "Use notElem instead of not (elem ...)"
                          , requiredImports: []
                        }
                    }
                ]
            Nothing -> []
        _ -> []
    _ -> []

  isNot :: ImportInfo -> Expr Void -> Boolean
  isNot imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "not" && hasValue imports "not"
  isNot _ _ = false

  getElemArgs :: ImportInfo -> Expr Void -> Maybe { x :: Expr Void, y :: Expr Void }
  getElemArgs imports (ExprApp fnExpr args) =
    case fnExpr of
      ExprIdent (QualifiedName { name: Ident name }) | name == "elem" && hasValue imports "elem" ->
        case NEA.toArray args of
          [AppTerm x, AppTerm y] -> Just { x, y }
          _ -> Nothing
      _ -> Nothing
  getElemArgs imports (ExprParens (Wrapped { value })) = getElemArgs imports value
  getElemArgs _ _ = Nothing

