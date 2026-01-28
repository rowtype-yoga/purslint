module Purslint.Rules.FunctorLaw where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Void (Void)
import Purslint.Imports (ImportInfo, hasValue, hasOp)
import Purslint.Print (printExpr)
import Purslint.Rule (Rule, RuleContext, mkRule)
import Purslint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, Operator(..), QualifiedName(..), Wrapped(..))

-- | Rule: map f (map g x) -> map (f <<< g) x (functor fusion)
-- | Also: f <$> (g <$> x) -> (f <<< g) <$> x
functorLawRule :: Rule
functorLawRule = mkRule (RuleId "FunctorLaw") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: map f (map g x)
    ExprApp fnExpr args
      | isMapLike imports fnExpr
      , [AppTerm fExpr, AppTerm innerExpr] <- NEA.toArray args ->
          checkInnerMap imports expr fExpr (unwrapParens innerExpr)
    -- Match: f <$> (g <$> x)
    ExprOp fExpr ops
      | hasOp imports "<$>"
      , [Tuple qn innerExpr] <- NEA.toArray ops
      , isMapOp imports qn ->
          checkInnerOp imports expr fExpr (unwrapParens innerExpr)
    _ -> []

  checkInnerMap :: ImportInfo -> Expr Void -> Expr Void -> Expr Void -> Array LintWarning
  checkInnerMap imports fullExpr fExpr inner = case inner of
    ExprApp innerFn innerArgs
      | isMapLike imports innerFn
      , [AppTerm gExpr, AppTerm xExpr] <- NEA.toArray innerArgs ->
          mkWarning fullExpr fExpr gExpr xExpr "map"
    _ -> []

  checkInnerOp :: ImportInfo -> Expr Void -> Expr Void -> Expr Void -> Array LintWarning
  checkInnerOp imports fullExpr fExpr inner = case inner of
    ExprOp gExpr innerOps
      | [Tuple innerQn xExpr] <- NEA.toArray innerOps
      , isMapOp imports innerQn ->
          mkWarning fullExpr fExpr gExpr xExpr "<$>"
    _ -> []

  mkWarning :: Expr Void -> Expr Void -> Expr Void -> Expr Void -> String -> Array LintWarning
  mkWarning fullExpr fExpr gExpr xExpr style =
    let
      fText = printExpr fExpr
      gText = printExpr gExpr
      xText = printExpr xExpr
      replacement = if style == "map" 
        then "map (" <> fText <> " <<< " <> gText <> ") " <> xText
        else "(" <> fText <> " <<< " <> gText <> ") <$> " <> xText
    in
      [ LintWarning
          { ruleId: RuleId "FunctorLaw"
          , message: WarningMessage "Functor law: fuse nested maps"
          , range: rangeOf fullExpr
          , severity: Warning
          , suggestion: Just $ Suggestion
              { replacement: ReplacementText replacement
              , description: SuggestionDescription $ style <> " f (" <> style <> " g x) can be fused to " <> style <> " (f <<< g) x"
                , requiredImports: []
              }
          }
      ]

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isMapLike :: ImportInfo -> Expr Void -> Boolean
  isMapLike imports (ExprIdent (QualifiedName { name: Ident name })) = 
    name == "map" && hasValue imports "map"
  isMapLike _ _ = false

  isMapOp :: ImportInfo -> QualifiedName Operator -> Boolean
  isMapOp imports (QualifiedName { name: Operator op }) = 
    op == "<$>" && hasOp imports "<$>"

