module Purelint.Rules.UseLastReverse where

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

-- | Rule: head (reverse x) -> last x
-- | Also: last (reverse x) -> head x
useLastReverseRule :: Rule
useLastReverseRule = mkRule (RuleId "UseLastReverse") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: head (reverse x) or last (reverse x)
    ExprApp fn args
      | isHead imports fn || isLast imports fn ->
        case NEA.toArray args of
          [AppTerm arg] ->
            case unwrapParens arg of
              ExprApp reverseFn reverseArgs
                | isReverse imports reverseFn ->
                  case NEA.toArray reverseArgs of
                    [AppTerm innerArg] ->
                      let
                        x = printExpr innerArg
                        isHeadCall = isHead imports fn
                        newFn = if isHeadCall then "last" else "head"
                        oldPattern = if isHeadCall then "head (reverse x)" else "last (reverse x)"
                      in
                        [ LintWarning
                            { ruleId: RuleId "UseLastReverse"
                            , message: WarningMessage $ oldPattern <> " can be simplified to " <> newFn <> " x"
                            , range: rangeOf expr
                            , severity: Warning
                            , suggestion: Just $ Suggestion
                                { replacement: ReplacementText (newFn <> " " <> x)
                                , description: SuggestionDescription $ "Use " <> newFn <> " instead"
                                }
                            }
                        ]
                    _ -> []
              _ -> []
          _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isHead :: ImportInfo -> Expr Void -> Boolean
  isHead imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "head" && hasValue imports "head"
  isHead _ _ = false

  isLast :: ImportInfo -> Expr Void -> Boolean
  isLast imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "last" && hasValue imports "last"
  isLast _ _ = false

  isReverse :: ImportInfo -> Expr Void -> Boolean
  isReverse imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "reverse" && hasValue imports "reverse"
  isReverse _ _ = false
