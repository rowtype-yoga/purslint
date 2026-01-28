module Purslint.Rules.MapIdentity where

import Prelude

import Data.Array as Array
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
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, Operator(..), QualifiedName(..))

-- | Rule: map identity x -> x
-- | Also: identity <$> x -> x, x <#> identity -> x
mapIdentityRule :: Rule
mapIdentityRule = run # mkRule (RuleId "MapIdentity")
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: map identity x
    ExprApp (ExprIdent qn) args
      | qn # isMap imports
      , [ AppTerm idExpr, AppTerm xExpr ] <- NEA.toArray args
      , idExpr # isIdentity imports ->
          [ LintWarning
              { ruleId: RuleId "MapIdentity"
              , message: WarningMessage "map identity is redundant"
              , range: rangeOf expr
              , severity: Warning
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText (printExpr xExpr)
                  , description: SuggestionDescription "map identity x can be simplified to x"
                    , requiredImports: []
                  }
              }
          ]
    -- Match: identity <$> x
    ExprOp lhs ops
      | isIdentity imports lhs
      , [ Tuple qn xExpr ] <- NEA.toArray ops
      , isMapOp imports qn ->
          [ LintWarning
              { ruleId: RuleId "MapIdentity"
              , message: WarningMessage "identity <$> is redundant"
              , range: rangeOf expr
              , severity: Warning
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText (printExpr xExpr)
                  , description: SuggestionDescription "identity <$> x can be simplified to x"
                    , requiredImports: []
                  }
              }
          ]
    -- Match: x <#> identity
    ExprOp lhs ops
      | [ Tuple qn idExpr ] <- NEA.toArray ops
      , isFlippedMapOp imports qn
      , isIdentity imports idExpr ->
          [ LintWarning
              { ruleId: RuleId "MapIdentity"
              , message: WarningMessage "<#> identity is redundant"
              , range: rangeOf expr
              , severity: Warning
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText (printExpr lhs)
                  , description: SuggestionDescription "x <#> identity can be simplified to x"
                    , requiredImports: []
                  }
              }
          ]
    _ -> []

  isMap :: ImportInfo -> QualifiedName Ident -> Boolean
  isMap imports (QualifiedName { name: Ident name }) =
    name == "map" && hasValue imports "map"

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

