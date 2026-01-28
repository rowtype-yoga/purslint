module Purslint.Rules.AlternativeLaw where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Purslint.Imports (ImportInfo, hasValue, hasOp)
import Purslint.Print (printExpr)
import Purslint.Rule (Rule, RuleContext, mkRule)
import Purslint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Expr(..), Ident(..), Module, Operator(..), QualifiedName(..), Proper(..), Wrapped(..))

-- | Alternative law rules:
-- | - empty <|> x -> x (left identity)
-- | - x <|> empty -> x (right identity)
alternativeLawRule :: Rule
alternativeLawRule = mkRule (RuleId "AlternativeLaw") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    ExprOp lhs ops ->
      case NEA.toArray ops of
        [Tuple (QualifiedName { name: Operator op }) rhs] 
          | op == "<|>" && hasOp imports "<|>" -> checkAlt imports expr lhs rhs
        _ -> []
    _ -> []

  checkAlt :: ImportInfo -> Expr Void -> Expr Void -> Expr Void -> Array LintWarning
  checkAlt imports fullExpr lhs rhs
    -- empty <|> x -> x
    | isEmpty imports lhs =
        [ LintWarning
            { ruleId: RuleId "AlternativeLaw"
            , message: WarningMessage "Alternative law: empty <|> x simplifies to x"
            , range: rangeOf fullExpr
            , severity: Warning
            , suggestion: Just $ Suggestion
                { replacement: ReplacementText (printExpr rhs)
                , description: SuggestionDescription "empty is the identity for <|> (left identity)"
                  , requiredImports: []
                }
            }
        ]
    -- x <|> empty -> x
    | isEmpty imports rhs =
        [ LintWarning
            { ruleId: RuleId "AlternativeLaw"
            , message: WarningMessage "Alternative law: x <|> empty simplifies to x"
            , range: rangeOf fullExpr
            , severity: Warning
            , suggestion: Just $ Suggestion
                { replacement: ReplacementText (printExpr lhs)
                , description: SuggestionDescription "empty is the identity for <|> (right identity)"
                  , requiredImports: []
                }
            }
        ]
    | otherwise = []

  isEmpty :: ImportInfo -> Expr Void -> Boolean
  isEmpty imports (ExprIdent (QualifiedName { name: Ident name })) = 
    (name == "empty" && hasValue imports "empty") ||
    (name == "mempty" && hasValue imports "mempty")
  isEmpty imports (ExprConstructor (QualifiedName { name: Proper "Nothing" })) = 
    hasValue imports "Nothing"
  isEmpty _ (ExprArray (Wrapped { value: Nothing })) = true -- []
  isEmpty _ _ = false

