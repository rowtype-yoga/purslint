module Purelint.Rules.UseUncurry where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Purelint.Imports (ImportInfo, hasValue)
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, Proper(..), QualifiedName(..), Separated(..), Wrapped(..))

-- | Rule: f (fst p) (snd p) -> uncurry f p
useUncurryRule :: Rule
useUncurryRule = mkRule (RuleId "UseUncurry") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: f (fst p) (snd p)
    ExprApp fn args ->
      case NEA.toArray args of
        [AppTerm arg1, AppTerm arg2] ->
          case getFstArg imports (unwrapParens arg1), getSndArg imports (unwrapParens arg2) of
            Just p1, Just p2 ->
              let
                p1Text = printExpr p1
                p2Text = printExpr p2
              in
                if p1Text == p2Text then
                  let
                    f = printExpr fn
                  in
                    [ LintWarning
                        { ruleId: RuleId "UseUncurry"
                        , message: WarningMessage "f (fst p) (snd p) can be simplified to uncurry f p"
                        , range: rangeOf expr
                        , severity: Hint
                        , suggestion: Just $ Suggestion
                            { replacement: ReplacementText ("uncurry " <> f <> " " <> p1Text)
                            , description: SuggestionDescription "Use uncurry instead of fst/snd"
                              , requiredImports: []
                            }
                        }
                    ]
                else []
            _, _ -> []
        _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isFst :: ImportInfo -> Expr Void -> Boolean
  isFst imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "fst" && hasValue imports "fst"
  isFst _ _ = false

  isSnd :: ImportInfo -> Expr Void -> Boolean
  isSnd imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "snd" && hasValue imports "snd"
  isSnd _ _ = false

  getFstArg :: ImportInfo -> Expr Void -> Maybe (Expr Void)
  getFstArg imports e =
    case e of
      ExprApp fstFn args
        | isFst imports fstFn ->
          case NEA.toArray args of
            [AppTerm arg] -> Just arg
            _ -> Nothing
      _ -> Nothing

  getSndArg :: ImportInfo -> Expr Void -> Maybe (Expr Void)
  getSndArg imports e =
    case e of
      ExprApp sndFn args
        | isSnd imports sndFn ->
          case NEA.toArray args of
            [AppTerm arg] -> Just arg
            _ -> Nothing
      _ -> Nothing

