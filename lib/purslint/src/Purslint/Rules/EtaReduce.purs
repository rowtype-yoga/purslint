module Purslint.Rules.EtaReduce where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Purslint.Print (printExpr)
import Purslint.Rule (Rule, RuleContext, mkRule)
import Purslint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Binder(..), Expr(..), Ident(..), Module, Name(..), QualifiedName(..), Wrapped(..))

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
    ExprLambda lambda -> lambda # checkLambda expr
    _ -> []

  checkLambda :: Expr Void -> _ -> Array LintWarning
  checkLambda fullExpr lambda =
    -- Check for single binder that's a simple variable
    case NEA.toArray lambda.binders of
      [ BinderVar (Name { name: Ident paramName }) ] ->
        -- Check if body is f x where x matches the param
        checkBody fullExpr paramName (unwrapParens lambda.body)
      _ -> []

  checkBody :: Expr Void -> String -> Expr Void -> Array LintWarning
  checkBody fullExpr paramName body =
    case body of
      ExprApp fn args -> do
        -- Check if the last argument is just our parameter
        let argsArr = NEA.toArray args
        case Array.last argsArr of
          Just (AppTerm (ExprIdent (QualifiedName { name: Ident argName })))
            | argName == paramName
            , not (fn # mentionsParam paramName)
            , not ((argsArr # Array.dropEnd 1) # Array.any (checkArg paramName)) -> do
                -- Can eta reduce
                let
                  remainingArgs = Array.dropEnd 1 argsArr
                  -- If there are remaining args, wrap in parens for safety
                  fnText =
                    if Array.null remainingArgs then printExpr fn
                    else "(" <> printExpr fn <> " " <> Array.intercalate " " (map printAppSpine remainingArgs) <> ")"
                [ LintWarning
                    { ruleId: RuleId "EtaReduce"
                    , message: WarningMessage "Lambda can be eta reduced"
                    , range: rangeOf fullExpr
                    , severity: Hint
                    , suggestion: Just $ Suggestion
                        { replacement: ReplacementText fnText
                        , description: SuggestionDescription $ "\\x -> f x can be simplified to f"
                          , requiredImports: []
                        }
                    }
                ]
          _ -> []
      _ -> []

  printAppSpine :: AppSpine Expr Void -> String
  printAppSpine (AppTerm e) = printExpr e
  printAppSpine (AppType _ _) = "@_"

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  -- Check if expression mentions the parameter (crude check)
  mentionsParam :: String -> Expr Void -> Boolean
  mentionsParam name (ExprIdent (QualifiedName { name: Ident n })) = n == name
  mentionsParam name (ExprApp fn args) =
    (fn # mentionsParam name) || ((NEA.toArray args) # Array.any (checkArg name))
  mentionsParam name (ExprParens (Wrapped { value })) = value # mentionsParam name
  mentionsParam _ _ = false

  checkArg :: String -> AppSpine Expr Void -> Boolean
  checkArg = case _, _ of
    name, (AppTerm e) -> e # mentionsParam name
    _, _ -> false

