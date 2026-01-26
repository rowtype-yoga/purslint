module Purelint.Rules.UseDollar where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Purelint.Print (printExpr, printType)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Module, Wrapped(..))

-- | Rule: Replace parens around last function argument with $
-- |
-- | f (expr) -> f $ expr
-- | map f (filter g xs) -> map f $ filter g xs
-- |
-- | Only applies when the inner expression is non-atomic (otherwise
-- | the parens are likely redundant and RedundantParens will catch it).
useDollarRule :: Rule
useDollarRule = mkRule (RuleId "UseDollar") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run _ctx = foldMapModule visitor
    where
    visitor = defaultMonoidalVisitor { onExpr = checkExpr }

  checkExpr :: Expr Void -> Array LintWarning
  checkExpr expr = case expr of
    -- Match function application where last arg is parenthesized
    -- f a b (expr) -> f a b $ expr
    ExprApp fn args ->
      case NEA.last args of
        AppTerm (ExprParens (Wrapped { value: inner }))
          -- Don't suggest $ for atomic expressions - parens are likely redundant anyway
          | not (isAtomicExpr inner)
          -- Don't suggest $ if inner is a section (would change semantics)
          , not (isSection inner) ->
              [ mkWarning expr fn args inner ]
        _ -> []

    _ -> []

  -- Check if expression is atomic (never needs parens)
  isAtomicExpr :: Expr Void -> Boolean
  isAtomicExpr = case _ of
    ExprIdent _ -> true
    ExprConstructor _ -> true
    ExprInt _ _ -> true
    ExprNumber _ _ -> true
    ExprString _ _ -> true
    ExprChar _ _ -> true
    ExprBoolean _ _ -> true
    ExprArray _ -> true
    ExprRecord _ -> true
    ExprParens _ -> true
    ExprRecordAccessor _ -> true
    _ -> false

  -- Check if expression is a section (would change semantics with $)
  isSection :: Expr Void -> Boolean
  isSection = case _ of
    ExprSection _ -> true
    ExprParens (Wrapped { value }) -> isSection value
    _ -> false

  mkWarning :: Expr Void -> Expr Void -> NEA.NonEmptyArray (AppSpine Expr Void) -> Expr Void -> LintWarning
  mkWarning outer fn args inner =
    let
      argsArray = NEA.toArray args
      initArgs = Array.dropEnd 1 argsArray
      
      prefix = case initArgs of
        [] -> printExpr fn
        _ -> printExpr fn <> " " <> (initArgs # map printAppSpine # Array.intercalate " ")
      
      replacement = prefix <> " $ " <> printExpr inner
    in
      LintWarning
        { ruleId: RuleId "UseDollar"
        , message: WarningMessage "Use $ instead of parentheses"
        , range: rangeOf outer
        , severity: Hint
        , suggestion: Just $ Suggestion
            { replacement: ReplacementText replacement
            , description: SuggestionDescription "Replace parentheses with $"
            }
        }

  printAppSpine :: AppSpine Expr Void -> String
  printAppSpine = case _ of
    AppTerm e -> printExpr e
    AppType _ t -> "@" <> printType t
