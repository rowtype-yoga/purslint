module Purelint.Rules.UseBindFlip where

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

-- | Rule: join (map f x) -> x >>= f
useBindFlipRule :: Rule
useBindFlipRule = mkRule (RuleId "UseBindFlip") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: join (map f x)
    ExprApp fnExpr args
      | isJoin imports fnExpr ->
        case NEA.toArray args of
          [AppTerm innerExpr] ->
            checkInnerMap imports expr (unwrapParens innerExpr)
          _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  checkInnerMap :: ImportInfo -> Expr Void -> Expr Void -> Array LintWarning
  checkInnerMap imports fullExpr innerExpr =
    case innerExpr of
      ExprApp mapFn mapArgs | isMap imports mapFn ->
        case NEA.toArray mapArgs of
          [AppTerm fExpr, AppTerm xExpr] ->
            let
              f = printExpr fExpr
              x = printExpr xExpr
            in
              [ LintWarning
                  { ruleId: RuleId "UseBindFlip"
                  , message: WarningMessage "join (map f x) can be simplified to x >>= f"
                  , range: rangeOf fullExpr
                  , severity: Warning
                  , suggestion: Just $ Suggestion
                      { replacement: ReplacementText (x <> " >>= " <> f)
                      , description: SuggestionDescription "Use >>= instead of join composed with map"
                      }
                  }
              ]
          _ -> []
      _ -> []

  isJoin :: ImportInfo -> Expr Void -> Boolean
  isJoin imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "join" && hasValue imports "join"
  isJoin _ _ = false

  isMap :: ImportInfo -> Expr Void -> Boolean
  isMap imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "map" && hasValue imports "map"
  isMap _ _ = false
