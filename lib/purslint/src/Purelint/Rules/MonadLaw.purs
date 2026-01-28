module Purelint.Rules.MonadLaw where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Purelint.Imports (ImportInfo, hasValue, hasOp)
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, Operator(..), QualifiedName(..))

-- | Monad law rules:
-- | - m >>= pure -> m (right identity, covered by RedundantBind)
-- | - pure a >>= f -> f a (left identity, covered by RedundantBind)
-- | - f =<< pure a -> f a (left identity, flipped)
-- | - pure =<< m -> m (right identity, flipped)
monadLawRule :: Rule
monadLawRule = mkRule (RuleId "MonadLaw") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: f =<< pure a
    ExprOp lhs ops ->
      case NEA.toArray ops of
        [Tuple (QualifiedName { name: Operator op }) rhs] 
          | op == "=<<" && hasOp imports ">>=" -> checkFlippedBind imports expr lhs rhs
        _ -> []
    _ -> []

  checkFlippedBind :: ImportInfo -> Expr Void -> Expr Void -> Expr Void -> Array LintWarning
  checkFlippedBind imports fullExpr f m =
    -- f =<< pure a -> f a
    case getPureArg imports m of
      Just arg ->
        let replacement = printExpr f <> " " <> wrapIfNeeded arg
        in
          [ LintWarning
              { ruleId: RuleId "MonadLaw"
              , message: WarningMessage "Monad law: f =<< pure a simplifies to f a"
              , range: rangeOf fullExpr
              , severity: Warning
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText replacement
                  , description: SuggestionDescription "Apply f directly to a (left identity)"
                    , requiredImports: []
                  }
              }
          ]
      Nothing -> 
        -- pure =<< m -> m
        if isPure imports f then
          [ LintWarning
              { ruleId: RuleId "MonadLaw"
              , message: WarningMessage "Monad law: pure =<< m simplifies to m"
              , range: rangeOf fullExpr
              , severity: Warning
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText (printExpr m)
                  , description: SuggestionDescription "pure =<< m is just m (right identity)"
                    , requiredImports: []
                  }
              }
          ]
        else []

  isPure :: ImportInfo -> Expr Void -> Boolean
  isPure imports (ExprIdent (QualifiedName { name: Ident name })) = 
    (name == "pure" && hasValue imports "pure") || (name == "return" && hasValue imports "pure")
  isPure _ _ = false

  getPureArg :: ImportInfo -> Expr Void -> Maybe (Expr Void)
  getPureArg imports (ExprApp fn args) 
    | isPure imports fn = 
        case NEA.toArray args of
          [AppTerm arg] -> Just arg
          _ -> Nothing
  getPureArg _ _ = Nothing

  wrapIfNeeded :: Expr Void -> String
  wrapIfNeeded e = case e of
    ExprIdent _ -> printExpr e
    ExprInt _ _ -> printExpr e
    ExprNumber _ _ -> printExpr e
    ExprString _ _ -> printExpr e
    ExprChar _ _ -> printExpr e
    ExprArray _ -> printExpr e
    ExprRecord _ -> printExpr e
    ExprParens _ -> printExpr e
    _ -> "(" <> printExpr e <> ")"

