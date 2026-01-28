module Purslint.Rules.EtaReduceDecl where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Void (Void)
import Purslint.Print (printExpr)
import Purslint.Rule (Rule, RuleContext, mkRule)
import Purslint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types as CST
import PureScript.CST.Types (AppSpine(..), Binder(..), Declaration(..), Expr(..), Guarded(..), Ident(..), Module, Name(..), QualifiedName(..), ValueBindingFields, Where(..), Wrapped(..))

-- | Rule: foo x = bar x -> foo = bar (top-level eta reduction)
etaReduceDeclRule :: Rule
etaReduceDeclRule = mkRule (RuleId "EtaReduceDecl") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run _ctx = foldMapModule visitor
    where
    visitor = defaultMonoidalVisitor { onDecl = checkDecl }

  checkDecl :: Declaration Void -> Array LintWarning
  checkDecl decl = case decl of
    DeclValue vb -> checkValueBinding vb
    _ -> []

  checkValueBinding :: ValueBindingFields Void -> Array LintWarning
  checkValueBinding vb =
    -- Check for pattern: foo x = expr x (single binder, unconditional)
    case vb.guarded of
      Unconditional _ (Where { expr, bindings: Nothing }) ->
        unwrapParens expr # checkEtaReducible vb.binders
      _ -> []

  checkEtaReducible :: Array (Binder Void) -> Expr Void -> Array LintWarning
  checkEtaReducible binders body
    | Just lastBinder@(BinderVar (Name { name: Ident paramName })) <- Array.last binders
    , ExprApp fn args <- body
    , argsArr <- NEA.toArray args
    , Just (AppTerm (ExprIdent (QualifiedName { name: Ident argName }))) <- Array.last argsArr
    , argName == paramName
    , not mentionsParam paramName fn
    , remainingArgs <- Array.dropEnd 1 argsArr
    , not (Array.any (checkArgMentions paramName) remainingArgs)
    , newRhs <-
        if Array.null remainingArgs then printExpr fn
        else printExpr fn <> " " <> Array.intercalate " " (map printAppSpine remainingArgs)
    , fullRange <- { start: (rangeOf lastBinder).start, end: (rangeOf body).end } =
        [ LintWarning
            { ruleId: RuleId "EtaReduceDecl"
            , message: WarningMessage "Declaration can be eta reduced"
            , range: fullRange
            , severity: Hint
            , suggestion: Just $ Suggestion
                { replacement: ReplacementText ("= " <> newRhs)
                , description: SuggestionDescription "Remove redundant parameter"
                  , requiredImports: []
                }
            }
        ]
    | otherwise = []

  printAppSpine :: AppSpine Expr Void -> String
  printAppSpine (AppTerm e) = printExpr e
  printAppSpine (AppType _ _) = "@_"

  checkArgMentions :: String -> AppSpine Expr Void -> Boolean
  checkArgMentions name (AppTerm e) = mentionsParam name e
  checkArgMentions _ _ = false

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  -- Check if expression mentions the parameter
  mentionsParam :: String -> Expr Void -> Boolean
  mentionsParam name (ExprIdent (QualifiedName { name: Ident n })) = n == name
  mentionsParam name (ExprApp fn args) =
    mentionsParam name fn || Array.any (checkArg name) (NEA.toArray args)
  mentionsParam name (ExprParens (Wrapped { value })) = mentionsParam name value
  mentionsParam _ _ = false

  checkArg :: String -> AppSpine Expr Void -> Boolean
  checkArg name (AppTerm e) = mentionsParam name e
  checkArg _ _ = false

