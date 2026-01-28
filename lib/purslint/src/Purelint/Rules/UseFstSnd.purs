module Purelint.Rules.UseFstSnd where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Binder(..), Expr(..), Ident(..), Module, Name(..), Proper(..), QualifiedName(..), Wrapped(..))

-- | Rule: \(Tuple x y) -> x -> fst
-- | Also: \(Tuple x y) -> y -> snd
useFstSndRule :: Rule
useFstSndRule = mkRule (RuleId "UseFstSnd") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run _ctx = foldMapModule visitor
    where
    visitor = defaultMonoidalVisitor { onExpr = checkExpr }

  checkExpr :: Expr Void -> Array LintWarning
  checkExpr expr = case expr of
    -- Match: \(Tuple x y) -> x or \(Tuple x y) -> y
    ExprLambda { binders, body } ->
      case NEA.toArray binders of
        [binder] ->
          case getTupleBinderNames binder of
            Just { first, second } ->
              case body of
                ExprIdent (QualifiedName { name: Ident bodyName }) 
                  | bodyName == first ->
                    [ LintWarning
                        { ruleId: RuleId "UseFstSnd"
                        , message: WarningMessage "\\(Tuple x y) -> x can be simplified to fst"
                        , range: rangeOf expr
                        , severity: Warning
                        , suggestion: Just $ Suggestion
                            { replacement: ReplacementText "fst"
                            , description: SuggestionDescription "Use fst instead of lambda extracting first element"
                              , requiredImports: []
                            }
                        }
                    ]
                  | bodyName == second ->
                    [ LintWarning
                        { ruleId: RuleId "UseFstSnd"
                        , message: WarningMessage "\\(Tuple x y) -> y can be simplified to snd"
                        , range: rangeOf expr
                        , severity: Warning
                        , suggestion: Just $ Suggestion
                            { replacement: ReplacementText "snd"
                            , description: SuggestionDescription "Use snd instead of lambda extracting second element"
                              , requiredImports: []
                            }
                        }
                    ]
                _ -> []
            Nothing -> []
        _ -> []
    _ -> []

  getTupleBinderNames :: Binder Void -> Maybe { first :: String, second :: String }
  getTupleBinderNames (BinderParens (Wrapped { value: inner })) = getTupleBinderNames inner
  getTupleBinderNames (BinderConstructor (QualifiedName { name: Proper ctorName }) binders) 
    | ctorName == "Tuple" =
      case binders of
        [firstBinder, secondBinder] ->
          case getBinderName firstBinder, getBinderName secondBinder of
            Just first, Just second -> Just { first, second }
            _, _ -> Nothing
        _ -> Nothing
  getTupleBinderNames _ = Nothing

  getBinderName :: Binder Void -> Maybe String
  getBinderName (BinderVar (Name { name: Ident name })) = Just name
  getBinderName (BinderWildcard _) = Nothing -- wildcards can't be referenced in body
  getBinderName (BinderParens (Wrapped { value: inner })) = getBinderName inner
  getBinderName _ = Nothing

