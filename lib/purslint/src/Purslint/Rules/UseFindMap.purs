module Purslint.Rules.UseFindMap where

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

-- | Rule: head (mapMaybe f x) -> findMap f x
useFindMapRule :: Rule
useFindMapRule = mkRule (RuleId "UseFindMap") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: head (mapMaybe f x)
    ExprApp fnExpr args
      | isHead imports fnExpr ->
        case NEA.toArray args of
          [AppTerm innerExpr] ->
            checkMapMaybe imports expr (unwrapParens innerExpr)
          _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  checkMapMaybe :: ImportInfo -> Expr Void -> Expr Void -> Array LintWarning
  checkMapMaybe imports fullExpr innerExpr =
    case innerExpr of
      ExprApp mapMaybeFn mapMaybeArgs | isMapMaybe imports mapMaybeFn ->
        case NEA.toArray mapMaybeArgs of
          [AppTerm fExpr, AppTerm xExpr] ->
            let
              f = printExpr fExpr
              x = printExpr xExpr
            in
              [ LintWarning
                  { ruleId: RuleId "UseFindMap"
                  , message: WarningMessage "head (mapMaybe f x) can be simplified to findMap f x"
                  , range: rangeOf fullExpr
                  , severity: Warning
                  , suggestion: Just $ Suggestion
                      { replacement: ReplacementText ("findMap " <> f <> " " <> x)
                      , description: SuggestionDescription "Use findMap instead of head composed with mapMaybe"
                      , requiredImports:
                          [ { moduleName: "Data.Array"
                            , importItem: Just "findMap"
                            , codeText: Just "findMap"
                            , qualifier: Nothing
                            }
                          ]
                      }
                  }
              ]
          _ -> []
      _ -> []

  isHead :: ImportInfo -> Expr Void -> Boolean
  isHead imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "head" && hasValue imports "head"
  isHead _ _ = false

  isMapMaybe :: ImportInfo -> Expr Void -> Boolean
  isMapMaybe imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "mapMaybe" && hasValue imports "mapMaybe"
  isMapMaybe _ _ = false

