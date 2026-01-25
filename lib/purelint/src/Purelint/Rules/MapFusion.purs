module Purelint.Rules.MapFusion where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Void (Void)
import Purelint.Imports (ImportInfo, hasValue, hasOp)
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, Operator(..), QualifiedName(..), Wrapped(..))

-- | Rule: map f (map g x) -> map (f <<< g) x
-- | Also: f <$> (g <$> x) -> (f <<< g) <$> x
-- | Also: x <#> g <#> f -> x <#> (g >>> f)
mapFusionRule :: Rule
mapFusionRule = mkRule (RuleId "MapFusion") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: map f (map g x)
    ExprApp fnExpr args ->
      case fnExpr of
        ExprIdent qn | isMap imports qn ->
          -- Check if we have at least 2 args and the second is map g x
          case NEA.toArray args of
            [AppTerm fExpr, AppTerm innerExpr] -> 
              checkInnerMap imports expr fExpr (unwrapParens innerExpr)
            _ -> []
        _ -> []
    -- Match: f <$> (g <$> x) or f <$> g <$> x
    ExprOp fExpr ops | NEA.length ops >= 1 ->
      checkOpChain imports expr fExpr (NEA.toArray ops)
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  -- Check function application form: map f (map g x)
  checkInnerMap :: ImportInfo -> Expr Void -> Expr Void -> Expr Void -> Array LintWarning
  checkInnerMap imports fullExpr fExpr innerExpr = 
    case innerExpr of
      ExprApp mapFn innerArgs | isMapApp imports mapFn ->
        case NEA.toArray innerArgs of
          [AppTerm gExpr, AppTerm xExpr] ->
            mkFusionWarning fullExpr fExpr gExpr xExpr "map"
          _ -> []
      -- Also check: map f (g <$> x)
      ExprOp gExpr innerOps ->
        case NEA.toArray innerOps of
          [Tuple qn xExpr] | isMapOp imports qn ->
            mkFusionWarning fullExpr fExpr gExpr xExpr "map"
          _ -> []
      _ -> []

  -- Check operator form: f <$> (g <$> x) or f <$> g <$> x
  checkOpChain :: ImportInfo -> Expr Void -> Expr Void -> Array (Tuple (QualifiedName Operator) (Expr Void)) -> Array LintWarning
  checkOpChain imports fullExpr fExpr ops = 
    case ops of
      -- f <$> (g <$> x) - inner is parenthesized
      [Tuple op1 innerExpr] | isMapOp imports op1 ->
        case unwrapParens innerExpr of
          ExprOp gExpr innerOps ->
            case NEA.toArray innerOps of
              [Tuple op2 xExpr] | isMapOp imports op2 ->
                mkFusionWarning fullExpr fExpr gExpr xExpr "<$>"
              _ -> []
          ExprApp mapFn innerArgs | isMapApp imports mapFn ->
            case NEA.toArray innerArgs of
              [AppTerm gExpr, AppTerm xExpr] ->
                mkFusionWarning fullExpr fExpr gExpr xExpr "<$>"
              _ -> []
          _ -> []
      -- x <#> g <#> f - flipped map chain
      [Tuple op1 gExpr, Tuple op2 fExpr'] | isFlippedMapOp imports op1 && isFlippedMapOp imports op2 ->
        let
          x = printExpr fExpr  -- fExpr is actually x in this case
          g = printExpr gExpr
          f = printExpr fExpr'
          replacement = x <> " <#> (" <> g <> " >>> " <> f <> ")"
        in
          [ LintWarning
              { ruleId: RuleId "MapFusion"
              , message: WarningMessage "Fuse nested maps using composition"
              , range: rangeOf fullExpr
              , severity: Hint
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText replacement
                  , description: SuggestionDescription "x <#> g <#> f can be replaced with x <#> (g >>> f)"
                  }
              }
          ]
      _ -> []

  mkFusionWarning :: Expr Void -> Expr Void -> Expr Void -> Expr Void -> String -> Array LintWarning
  mkFusionWarning fullExpr fExpr gExpr xExpr style =
    let
      f = printExpr fExpr
      g = printExpr gExpr
      x = printExpr xExpr
      replacement = case style of
        "<$>" -> "(" <> f <> " <<< " <> g <> ") <$> " <> x
        _ -> "map (" <> f <> " <<< " <> g <> ") " <> x
    in
      [ LintWarning
          { ruleId: RuleId "MapFusion"
          , message: WarningMessage "Fuse nested maps using composition"
          , range: rangeOf fullExpr
          , severity: Hint
          , suggestion: Just $ Suggestion
              { replacement: ReplacementText replacement
              , description: SuggestionDescription "Nested maps can be fused with composition"
              }
          }
      ]

  isMapApp :: ImportInfo -> Expr Void -> Boolean
  isMapApp imports (ExprIdent qn) = isMap imports qn
  isMapApp _ _ = false

  isMap :: ImportInfo -> QualifiedName Ident -> Boolean
  isMap imports (QualifiedName { name: Ident name }) = 
    (name == "map" && hasValue imports "map") || (name == "fmap" && hasValue imports "fmap")

  isMapOp :: ImportInfo -> QualifiedName Operator -> Boolean
  isMapOp imports (QualifiedName { name: Operator op }) = 
    (op == "<$>" || op == "<$$>") && hasOp imports "<$>"

  isFlippedMapOp :: ImportInfo -> QualifiedName Operator -> Boolean
  isFlippedMapOp imports (QualifiedName { name: Operator op }) = 
    (op == "<#>" || op == "<##>") && hasOp imports "<#>"
