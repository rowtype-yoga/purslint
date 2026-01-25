module Purelint.Rules.UseReplicate where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Purelint.Imports (ImportInfo, hasValue)
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, QualifiedName(..), Wrapped(..))

-- | Rule: take n (repeat x) -> replicate n x
useReplicateRule :: Rule
useReplicateRule = mkRule (RuleId "UseReplicate") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: take n (repeat x)
    ExprApp fn args
      | isTake imports fn ->
        case NEA.toArray args of
          [AppTerm nArg, AppTerm repeatArg] ->
            case getRepeatArg imports (unwrapParens repeatArg) of
              Just x ->
                let
                  n = printExpr nArg
                  xText = printExpr x
                in
                  [ LintWarning
                      { ruleId: RuleId "UseReplicate"
                      , message: WarningMessage "take n (repeat x) can be simplified to replicate n x"
                      , range: rangeOf expr
                      , severity: Warning
                      , suggestion: Just $ Suggestion
                          { replacement: ReplacementText ("replicate " <> n <> " " <> xText)
                          , description: SuggestionDescription "Use replicate instead of take n (repeat x)"
                          }
                      }
                  ]
              Nothing -> []
          _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isTake :: ImportInfo -> Expr Void -> Boolean
  isTake imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "take" && hasValue imports "take"
  isTake _ _ = false

  isRepeat :: ImportInfo -> Expr Void -> Boolean
  isRepeat imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "repeat" && hasValue imports "repeat"
  isRepeat _ _ = false

  getRepeatArg :: ImportInfo -> Expr Void -> Maybe (Expr Void)
  getRepeatArg imports e =
    case e of
      ExprApp repeatFn args
        | isRepeat imports repeatFn ->
          case NEA.toArray args of
            [AppTerm arg] -> Just arg
            _ -> Nothing
      _ -> Nothing
