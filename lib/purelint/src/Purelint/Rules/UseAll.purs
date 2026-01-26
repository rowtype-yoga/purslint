module Purelint.Rules.UseAll where

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

-- | Rule: and (map f x) -> all f x
useAllRule :: Rule
useAllRule = mkRule (RuleId "UseAll") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: and (map f x)
    ExprApp fnExpr args | isAnd imports fnExpr ->
      case NEA.toArray args of
        [AppTerm innerExpr] ->
          case unwrapParens innerExpr of
            ExprApp mapFn mapArgs | isMapLike imports mapFn ->
              case NEA.toArray mapArgs of
                [AppTerm fExpr, AppTerm xExpr] ->
                  let 
                    fText = printExpr fExpr
                    xText = printExpr xExpr
                  in
                    [ LintWarning
                        { ruleId: RuleId "UseAll"
                        , message: WarningMessage "Use all instead of and (map f x)"
                        , range: rangeOf expr
                        , severity: Warning
                        , suggestion: Just $ Suggestion
                            { replacement: ReplacementText ("all " <> fText <> " " <> xText)
                            , description: SuggestionDescription "and (map f x) can be simplified to all f x"
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

  isAnd :: ImportInfo -> Expr Void -> Boolean
  isAnd imports (ExprIdent (QualifiedName { name: Ident name })) = 
    name == "and" && hasValue imports "and"
  isAnd _ _ = false

  isMapLike :: ImportInfo -> Expr Void -> Boolean
  isMapLike imports (ExprIdent (QualifiedName { name: Ident name })) = 
    name == "map" && hasValue imports "map"
  isMapLike _ _ = false
