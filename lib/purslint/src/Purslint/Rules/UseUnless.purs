module Purslint.Rules.UseUnless where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Purslint.Imports (ImportInfo, hasValue)
import Purslint.Print (printExpr)
import Purslint.Rule (Rule, RuleContext, mkRule)
import Purslint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Expr(..), Ident(..), Module, QualifiedName(..))

-- | Rule: if x then pure unit else y -> unless x y
useUnlessRule :: Rule
useUnlessRule = mkRule (RuleId "UseUnless") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: if cond then pure unit else action
    ExprIf { cond, true: trueExpr, false: falseExpr } 
      | isPureUnit imports trueExpr ->
        let condText = printExpr cond
            actionText = printExpr falseExpr
        in
          [ LintWarning
              { ruleId: RuleId "UseUnless"
              , message: WarningMessage "if x then pure unit else y can be simplified to unless x y"
              , range: rangeOf expr
              , severity: Warning
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText ("unless " <> condText <> " " <> actionText)
                  , description: SuggestionDescription "Use unless instead of if/else with pure unit"
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

