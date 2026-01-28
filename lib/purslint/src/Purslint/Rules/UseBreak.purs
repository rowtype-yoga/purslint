module Purslint.Rules.UseBreak where

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

-- | Rule: span (not <<< p) -> break p (composition form)
-- | Also: span (\x -> not (p x)) -> break p (lambda form)
useBreakRule :: Rule
useBreakRule = mkRule (RuleId "UseBreak") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: span (not <<< p) x
    ExprApp fn args
      | isSpan imports fn ->
          case NEA.toArray args of
            [ AppTerm predArg, AppTerm xArg ] ->
              case getNotComposedPred imports predArg of
                Just p ->
                  let
                    pText = printExpr p
                    x = printExpr xArg
                  in
                    [ LintWarning
                        { ruleId: RuleId "UseBreak"
                        , message: WarningMessage "span (not <<< p) can be simplified to break p"
                        , range: rangeOf expr
                        , severity: Hint
                        , suggestion: Just $ Suggestion
                            { replacement: ReplacementText ("break " <> pText <> " " <> x)
                            , description: SuggestionDescription "Use break instead of span with negated predicate"
                              , requiredImports: []
                            }
                        }
                    ]
                Nothing -> []
            [ AppTerm predArg ] ->
              case getNotComposedPred imports predArg of
                Just p ->
                  let
                    pText = printExpr p
                  in
                    [ LintWarning
                        { ruleId: RuleId "UseBreak"
                        , message: WarningMessage "span (not <<< p) can be simplified to break p"
                        , range: rangeOf expr
                        , severity: Hint
                        , suggestion: Just $ Suggestion
                            { replacement: ReplacementText ("break " <> pText)
                            , description: SuggestionDescription "Use break instead of span with negated predicate"
                              , requiredImports: []
                            }
                        }
                    ]
                Nothing -> []
            _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isSpan :: ImportInfo -> Expr Void -> Boolean
  isSpan imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "span" && hasValue imports "span"
  isSpan _ _ = false

  isNot :: ImportInfo -> Expr Void -> Boolean
  isNot imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "not" && hasValue imports "not"
  isNot _ _ = false

  -- Check for (not <<< p) pattern
  getNotComposedPred :: ImportInfo -> Expr Void -> Maybe (Expr Void)
  getNotComposedPred imports predExpr =
    case unwrapParens predExpr of
      ExprApp notFn args
        | isNot imports notFn ->
            case NEA.toArray args of
              [ AppTerm p ] -> Just p
              _ -> Nothing
      _ -> Nothing

