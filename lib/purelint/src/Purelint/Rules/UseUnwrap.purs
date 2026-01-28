module Purelint.Rules.UseUnwrap where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, QualifiedName(..))

-- | Rule: unwrap x -> un ?Constructor x
-- | Suggests replacing unwrap with typed hole to trigger type-directed search
useUnwrapRule :: Rule
useUnwrapRule = mkRule (RuleId "UseUnwrap") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run _ctx = foldMapModule visitor
    where
    visitor = defaultMonoidalVisitor { onExpr = checkExpr }

  checkExpr :: Expr Void -> Array LintWarning
  checkExpr expr = case expr of
    -- Match: unwrap x
    ExprApp fn args
      | isUnwrap fn
      , [AppTerm arg] <- NEA.toArray args
      -> let
           argText = printExpr arg
           replacement = "un ?Constructor " <> argText
         in
           [ LintWarning
               { ruleId: RuleId "UseUnwrap"
               , message: WarningMessage "unwrap can be replaced with un ?Constructor for type-directed search"
               , range: rangeOf expr
               , severity: Hint
               , suggestion: Just $ Suggestion
                   { replacement: ReplacementText replacement
                   , description: SuggestionDescription "Use un with typed hole to find the right accessor"
                     , requiredImports: []
                   }
               }
           ]
    -- Match: unwrap (partial application)
    ExprIdent qn
      | isUnwrapIdent qn
      -> [ LintWarning
             { ruleId: RuleId "UseUnwrap"
             , message: WarningMessage "unwrap can be replaced with un ?Constructor for type-directed search"
             , range: rangeOf expr
             , severity: Hint
             , suggestion: Just $ Suggestion
                 { replacement: ReplacementText "un ?Constructor"
                 , description: SuggestionDescription "Use un with typed hole to find the right accessor"
                   , requiredImports: []
                 }
             }
         ]
    _ -> []

  isUnwrap :: Expr Void -> Boolean
  isUnwrap (ExprIdent qn) = isUnwrapIdent qn
  isUnwrap _ = false

  isUnwrapIdent :: QualifiedName Ident -> Boolean
  isUnwrapIdent (QualifiedName { name: Ident name }) = name == "unwrap"

