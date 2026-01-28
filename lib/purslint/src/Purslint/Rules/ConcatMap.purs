module Purslint.Rules.ConcatMap where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Purslint.Imports (ImportInfo, hasValue)
import Purslint.Print (printExpr)
import Purslint.Rule (Rule, RuleContext, mkRule)
import Purslint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, QualifiedName(..), Wrapped(..))

-- | Rule: concat (map f x) -> concatMap f x
-- | Also: join (map f x) -> (=<<) f x (for Monads)
concatMapRule :: Rule
concatMapRule = mkRule (RuleId "ConcatMap") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: concat (map f x) or join (map f x)
    ExprApp (ExprIdent qn) args
      | qn # isConcatOrJoin imports
      , AppTerm innerExpr <- NEA.head args ->
          checkInnerMap imports expr qn (unwrapParens innerExpr)
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  checkInnerMap :: ImportInfo -> Expr Void -> QualifiedName Ident -> Expr Void -> Array LintWarning
  checkInnerMap imports fullExpr fnQn innerExpr =
    case innerExpr of
      ExprApp mapFn innerArgs
        | isMapApp imports mapFn
        , [ AppTerm fExpr, AppTerm xExpr ] <- NEA.toArray innerArgs ->
            let
              fnName = getIdentName fnQn
              replacementFn = if fnName == "concat" then "concatMap" else "(=<<)"
              f = printExpr fExpr
              x = printExpr xExpr
              replacement = replacementFn <> " " <> f <> " " <> x
              msg = fnName <> " (map f x) can be replaced with " <> replacementFn <> " f x"
            in
              [ LintWarning
                  { ruleId: RuleId "ConcatMap"
                  , message: WarningMessage $ "Use " <> replacementFn <> " instead of " <> fnName <> " composed with map"
                  , range: rangeOf fullExpr
                  , severity: Hint
                  , suggestion: Just $ Suggestion
                      { replacement: ReplacementText replacement
                      , description: SuggestionDescription msg
                        , requiredImports: []
                      }
                  }
              ]
      _ -> []

  isMapApp :: ImportInfo -> Expr Void -> Boolean
  isMapApp imports (ExprIdent qn) = isMap imports qn
  isMapApp _ _ = false

  isMap :: ImportInfo -> QualifiedName Ident -> Boolean
  isMap imports (QualifiedName { name: Ident name }) =
    name == "map" && hasValue imports "map"

  isConcatOrJoin :: ImportInfo -> QualifiedName Ident -> Boolean
  isConcatOrJoin imports (QualifiedName { name: Ident name }) =
    (name == "concat" && hasValue imports "concat") || (name == "join" && hasValue imports "join")

  getIdentName :: QualifiedName Ident -> String
  getIdentName (QualifiedName { name: Ident name }) = name

