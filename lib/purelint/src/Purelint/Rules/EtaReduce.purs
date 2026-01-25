module Purelint.Rules.EtaReduce where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Binder(..), Expr(..), Ident(..), Module, Name(..), QualifiedName(..), Wrapped(..))
import Data.Void (Void)

-- | Rule: \x -> f x -> f (eta reduction)
-- | Detects simple lambda functions that can be eta reduced
etaReduceRule :: Rule
etaReduceRule = mkRule (RuleId "EtaReduce") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run _ctx = foldMapModule visitor
    where
    visitor = defaultMonoidalVisitor { onExpr = checkExpr }

  checkExpr :: Expr Void -> Array LintWarning
  checkExpr expr = case expr of
    -- Match: \x -> f x
    ExprLambda lambda ->
      checkLambda expr lambda
    _ -> []

  checkLambda :: Expr Void -> _ -> Array LintWarning
  checkLambda fullExpr lambda =
    -- Check for single binder that's a simple variable
    case NEA.toArray lambda.binders of
      [BinderVar (Name { name: Ident paramName })] ->
        -- Check if body is f x where x matches the param
        checkBody fullExpr paramName (unwrapParens lambda.body)
      _ -> []

  checkBody :: Expr Void -> String -> Expr Void -> Array LintWarning
  checkBody fullExpr paramName body =
    case body of
      ExprApp fn args ->
        -- Check if the last argument is just our parameter
        let argsArr = NEA.toArray args in
        case Array.last argsArr of
          Just (AppTerm (ExprIdent (QualifiedName { name: Ident argName }))) 
            | argName == paramName && not (mentionsParam paramName fn) && Array.length argsArr == 1 ->
              -- Single arg case: \x -> f x -> f
              let fnText = printExpr fn
              in
                [ LintWarning
                    { ruleId: RuleId "EtaReduce"
                    , message: WarningMessage "Lambda can be eta reduced"
                    , range: rangeOf fullExpr
                    , severity: Hint
                    , suggestion: Just $ Suggestion
                        { replacement: ReplacementText fnText
                        , description: SuggestionDescription $ "\\x -> f x can be simplified to f"
                        }
                    }
                ]
          _ -> []
      _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  -- Check if expression mentions the parameter (crude check)
  mentionsParam :: String -> Expr Void -> Boolean
  mentionsParam name (ExprIdent (QualifiedName { name: Ident n })) = n == name
  mentionsParam name (ExprApp fn args) = 
    mentionsParam name fn || Array.any (checkArg name) (NEA.toArray args)
  mentionsParam name (ExprParens (Wrapped { value })) = mentionsParam name value
  mentionsParam _ _ = false

  checkArg :: String -> AppSpine Expr Void -> Boolean
  checkArg name (AppTerm e) = mentionsParam name e
  checkArg _ _ = false
