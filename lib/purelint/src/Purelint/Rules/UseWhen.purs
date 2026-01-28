module Purelint.Rules.UseWhen where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Purelint.Imports (ImportInfo, hasValue)
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Expr(..), Ident(..), Module, QualifiedName(..))

-- | Rule: if x then y else pure unit -> when x y
useWhenRule :: Rule
useWhenRule = mkRule (RuleId "UseWhen") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: if cond then action else pure unit
    ExprIf { cond, true: trueExpr, false: falseExpr } 
      | isPureUnit imports falseExpr ->
        let condText = printExpr cond
            actionText = printExpr trueExpr
        in
          [ LintWarning
              { ruleId: RuleId "UseWhen"
              , message: WarningMessage "if x then y else pure unit can be simplified to when x y"
              , range: rangeOf expr
              , severity: Warning
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText ("when " <> condText <> " " <> actionText)
                  , description: SuggestionDescription "Use when instead of if/else with pure unit"
                    , requiredImports: []
                  }
              }
          ]
    _ -> []

  isPureUnit :: ImportInfo -> Expr Void -> Boolean
  isPureUnit imports (ExprApp fnExpr _) = isPure imports fnExpr
  isPureUnit imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "unit" || (name == "pure" && hasValue imports "pure")
  isPureUnit _ _ = false

  isPure :: ImportInfo -> Expr Void -> Boolean  
  isPure imports (ExprIdent (QualifiedName { name: Ident name })) =
    (name == "pure" && hasValue imports "pure") || (name == "return" && hasValue imports "return")
  isPure _ _ = false

