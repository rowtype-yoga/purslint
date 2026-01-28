module Purelint.Rules.UseApplyFlipped where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Purelint.Print (printExprNoComments)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Module, Wrapped(..))

-- | Rule: f arg -> arg # f (when f is complex like a lambda or partial application)
-- | Suggests using # for better readability when the function is more complex than the argument
useApplyFlippedRule :: Rule
useApplyFlippedRule = mkRule (RuleId "UseApplyFlipped") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run _ctx = foldMapModule visitor
    where
    visitor = defaultMonoidalVisitor { onExpr = checkExpr }

  checkExpr :: Expr Void -> Array LintWarning
  checkExpr expr = case expr of
    -- Match: f arg1 arg2  -->  arg2 # f arg1
    -- Triggers for any 2-arg application
    ExprApp fn args
      | [AppTerm firstArg, AppTerm lastArg] <- NEA.toArray args
      -> let
           fnText = printExprNoComments fn
           -- For lambdas, unwrap parens so we get `\x -> ...` not `(\x -> ...)`
           firstArgText = case unwrapParens firstArg of
             l@(ExprLambda _) -> printExprNoComments l
             _ -> printExprNoComments firstArg
           lastArgText = printExprNoComments lastArg
           replacement = lastArgText <> " # " <> fnText <> " " <> firstArgText
         in
           [ LintWarning
               { ruleId: RuleId "UseApplyFlipped"
               , message: WarningMessage "Consider using # for better readability"
               , range: rangeOf expr
               , severity: Refactor
               , suggestion: Just $ Suggestion
                   { replacement: ReplacementText replacement
                   , description: SuggestionDescription ("Rewrite as: " <> replacement)
                     , requiredImports: []
                   }
               }
           ]
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

