module Purelint.Rules.FmapId where

import Prelude

import Data.Array as Array
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
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, Operator(..), QualifiedName(..))

-- | Rule: map identity x -> x
-- | Also: fmap identity x -> x, identity <$> x -> x, x <#> identity -> x
fmapIdRule :: Rule
fmapIdRule = mkRule (RuleId "FmapId") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: map identity x or fmap identity x
    ExprApp fnExpr args ->
      case fnExpr of
        ExprIdent qn | isMapLike imports qn ->
          case NEA.toArray args of
            [AppTerm idExpr, AppTerm xExpr] | isIdentity imports idExpr ->
              let xText = printExpr xExpr
              in
                [ LintWarning
                    { ruleId: RuleId "FmapId"
                    , message: WarningMessage "map identity is redundant"
                    , range: rangeOf expr
                    , severity: Warning
                    , suggestion: Just $ Suggestion
                        { replacement: ReplacementText xText
                        , description: SuggestionDescription "map identity x can be simplified to x"
                        }
                    }
                ]
            _ -> []
        _ -> []
    -- Match: identity <$> x
    ExprOp lhs ops | isIdentity imports lhs ->
      case NEA.toArray ops of
        [Tuple qn xExpr] | isMapOp imports qn ->
          let xText = printExpr xExpr
          in
            [ LintWarning
                { ruleId: RuleId "FmapId"
                , message: WarningMessage "identity <$> is redundant"
                , range: rangeOf expr
                , severity: Warning
                , suggestion: Just $ Suggestion
                    { replacement: ReplacementText xText
                    , description: SuggestionDescription "identity <$> x can be simplified to x"
                    }
                }
            ]
        _ -> []
    -- Match: x <#> identity
    ExprOp lhs ops ->
      case NEA.toArray ops of
        [Tuple qn idExpr] | isFlippedMapOp imports qn && isIdentity imports idExpr ->
          let xText = printExpr lhs
          in
            [ LintWarning
                { ruleId: RuleId "FmapId"
                , message: WarningMessage "<#> identity is redundant"
                , range: rangeOf expr
                , severity: Warning
                , suggestion: Just $ Suggestion
                    { replacement: ReplacementText xText
                    , description: SuggestionDescription "x <#> identity can be simplified to x"
                    }
                }
            ]
        _ -> []
    _ -> []

  isMapLike :: ImportInfo -> QualifiedName Ident -> Boolean
  isMapLike imports (QualifiedName { name: Ident name }) = 
    (name == "map" && hasValue imports "map") || (name == "fmap" && hasValue imports "fmap")

  isMapOp :: ImportInfo -> QualifiedName Operator -> Boolean
  isMapOp imports (QualifiedName { name: Operator op }) = 
    (op == "<$>" || op == "<$$>") && hasOp imports "<$>"

  isFlippedMapOp :: ImportInfo -> QualifiedName Operator -> Boolean
  isFlippedMapOp imports (QualifiedName { name: Operator op }) = 
    (op == "<#>" || op == "<##>") && hasOp imports "<#>"

  isIdentity :: ImportInfo -> Expr Void -> Boolean
  isIdentity imports (ExprIdent (QualifiedName { name: Ident name })) = 
    (name == "identity" && hasValue imports "identity") || (name == "id" && hasValue imports "id")
  isIdentity _ _ = false
