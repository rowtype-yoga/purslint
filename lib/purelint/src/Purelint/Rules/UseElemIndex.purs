module Purelint.Rules.UseElemIndex where

import Prelude

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
import PureScript.CST.Types (AppSpine(..), Binder(..), Expr(..), Ident(..), Module, Name(..), Operator(..), QualifiedName(..), Wrapped(..))

-- | Rule: findIndex (== a) -> elemIndex a
-- | Also: findIndex (\x -> x == a) -> elemIndex a
useElemIndexRule :: Rule
useElemIndexRule = mkRule (RuleId "UseElemIndex") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: findIndex (== a) xs or findIndex (\x -> x == a)
    ExprApp fn args
      | isFindIndex imports fn ->
        case NEA.toArray args of
          [AppTerm predArg] ->
            case getEqArg imports (unwrapParens predArg) of
              Just a ->
                let aText = printExpr a
                in
                  [ LintWarning
                      { ruleId: RuleId "UseElemIndex"
                      , message: WarningMessage "findIndex (== a) can be simplified to elemIndex a"
                      , range: rangeOf expr
                      , severity: Warning
                      , suggestion: Just $ Suggestion
                          { replacement: ReplacementText ("elemIndex " <> aText)
                          , description: SuggestionDescription "Use elemIndex instead of findIndex (== a)"
                          }
                      }
                  ]
              Nothing -> []
          [AppTerm predArg, AppTerm xArg] ->
            case getEqArg imports (unwrapParens predArg) of
              Just a ->
                let
                  aText = printExpr a
                  x = printExpr xArg
                in
                  [ LintWarning
                      { ruleId: RuleId "UseElemIndex"
                      , message: WarningMessage "findIndex (== a) xs can be simplified to elemIndex a xs"
                      , range: rangeOf expr
                      , severity: Warning
                      , suggestion: Just $ Suggestion
                          { replacement: ReplacementText ("elemIndex " <> aText <> " " <> x)
                          , description: SuggestionDescription "Use elemIndex instead of findIndex (== a)"
                          }
                      }
                  ]
              Nothing -> []
          _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isFindIndex :: ImportInfo -> Expr Void -> Boolean
  isFindIndex imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "findIndex" && hasValue imports "findIndex"
  isFindIndex _ _ = false

  -- Check for (== a) section or (\x -> x == a) lambda
  getEqArg :: ImportInfo -> Expr Void -> Maybe (Expr Void)
  getEqArg imports e = case e of
    -- Section: (== a)
    ExprSection _ ->
      -- Sections are tricky to parse, skip for now
      Nothing
    -- Operator section in parens: (== a)
    ExprOp lhs ops
      | hasOp imports "==" ->
        case NEA.toArray ops of
          [Tuple (QualifiedName { name: Operator "==" }) rhs] ->
            -- This would be `lhs == rhs`, not a section
            -- Sections appear differently
            Nothing
          _ -> Nothing
    -- Lambda: \x -> x == a
    ExprLambda { binders, body } ->
      case NEA.toArray binders of
        [binder] ->
          case getBinderName binder of
            Just varName ->
              case body of
                ExprOp lhs ops
                  | hasOp imports "==" ->
                    case NEA.toArray ops of
                      [Tuple (QualifiedName { name: Operator "==" }) rhs] ->
                        -- Check if lhs is the bound variable
                        case lhs of
                          ExprIdent (QualifiedName { name: Ident name })
                            | name == varName -> Just rhs
                          _ ->
                            -- Check if rhs is the bound variable
                            case rhs of
                              ExprIdent (QualifiedName { name: Ident name })
                                | name == varName -> Just lhs
                              _ -> Nothing
                      _ -> Nothing
                _ -> Nothing
            Nothing -> Nothing
        _ -> Nothing
    _ -> Nothing

  getBinderName :: Binder Void -> Maybe String
  getBinderName (BinderVar (Name { name: Ident name })) = Just name
  getBinderName (BinderParens (Wrapped { value: inner })) = getBinderName inner
  getBinderName _ = Nothing
