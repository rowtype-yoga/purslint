module Purslint.Rules.UseMinimumSort where

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

-- | Rule: head (sort x) -> minimum x
-- | Also: last (sort x) -> maximum x
useMinimumSortRule :: Rule
useMinimumSortRule = mkRule (RuleId "UseMinimumSort") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: head (sort x) or last (sort x)
    ExprApp fn args
      | isHead imports fn || isLast imports fn ->
        case NEA.toArray args of
          [AppTerm arg] ->
            case unwrapParens arg of
              ExprApp sortFn sortArgs
                | isSort imports sortFn ->
                  case NEA.toArray sortArgs of
                    [AppTerm innerArg] ->
                      let
                        x = printExpr innerArg
                        isHeadCall = isHead imports fn
                        newFn = if isHeadCall then "minimum" else "maximum"
                        oldPattern = if isHeadCall then "head (sort x)" else "last (sort x)"
                      in
                        [ LintWarning
                            { ruleId: RuleId "UseMinimumSort"
                            , message: WarningMessage $ oldPattern <> " can be simplified to " <> newFn <> " x"
                            , range: rangeOf expr
                            , severity: Warning
                            , suggestion: Just $ Suggestion
                                { replacement: ReplacementText (newFn <> " " <> x)
                                , description: SuggestionDescription $ "Use " <> newFn <> " instead of sorting"
                                  , requiredImports: []
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

  isSort :: ImportInfo -> Expr Void -> Boolean
  isSort imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "sort" && hasValue imports "sort"
  isSort _ _ = false

