module Purelint.Rules.CollapseLambdas where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.String as String
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Binder(..), Expr(..), Ident(..), Module, Name(..))

-- | Rule: \x -> \y -> body -> \x y -> body
collapseLambdasRule :: Rule
collapseLambdasRule = run # mkRule (RuleId "CollapseLambdas")
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run _ctx = foldMapModule visitor
    where
    visitor = defaultMonoidalVisitor { onExpr = checkExpr }

  checkExpr :: Expr Void -> Array LintWarning
  checkExpr expr = case expr of
    ExprLambda lambda ->
      -- Check if body is another lambda
      case lambda.body of
        ExprLambda innerLambda -> do
          let
            outerBinders = NEA.toArray lambda.binders
            innerBinders = NEA.toArray innerLambda.binders
            allBinders = outerBinders <> innerBinders
            binderNames = Array.mapMaybe getBinderName allBinders
            bodyText = printExpr innerLambda.body
            replacement = "\\" <> String.joinWith " " binderNames <> " -> " <> bodyText
          [ LintWarning
              { ruleId: RuleId "CollapseLambdas"
              , message: WarningMessage "Collapse nested lambdas"
              , range: rangeOf expr
              , severity: Hint
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText replacement
                  , description: SuggestionDescription "\\x -> \\y -> body can be simplified to \\x y -> body"
                  }
              }
          ]
        _ -> []
    _ -> []

  getBinderName :: Binder Void -> Maybe String
  getBinderName (BinderVar (Name { name: Ident n })) = Just n
  getBinderName _ = Nothing
