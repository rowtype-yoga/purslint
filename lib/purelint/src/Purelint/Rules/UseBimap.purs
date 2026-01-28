module Purelint.Rules.UseBimap where

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
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, QualifiedName(..), Wrapped(..))

-- | Rule: bimap id g -> second g
-- | Also: bimap f id -> first f
-- | Also: first f (second g x) -> bimap f g x
useBimapRule :: Rule
useBimapRule = mkRule (RuleId "UseBimap") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: bimap id g or bimap f id
    ExprApp fn args
      | isBimap imports fn ->
          case NEA.toArray args of
            [ AppTerm fArg, AppTerm gArg ] ->
              let
                fIsId = isIdentity imports fArg
                gIsId = isIdentity imports gArg
              in
                if fIsId && not gIsId then
                  let
                    g = printExpr gArg
                  in
                    [ LintWarning
                        { ruleId: RuleId "UseBimap"
                        , message: WarningMessage "bimap id g can be simplified to second g"
                        , range: rangeOf expr
                        , severity: Warning
                        , suggestion: Just $ Suggestion
                            { replacement: ReplacementText ("second " <> g)
                            , description: SuggestionDescription "Use second instead of bimap id"
                              , requiredImports: []
                            }
                        }
                    ]
                else if gIsId && not fIsId then
                  let
                    f = printExpr fArg
                  in
                    [ LintWarning
                        { ruleId: RuleId "UseBimap"
                        , message: WarningMessage "bimap f id can be simplified to first f"
                        , range: rangeOf expr
                        , severity: Warning
                        , suggestion: Just $ Suggestion
                            { replacement: ReplacementText ("first " <> f)
                            , description: SuggestionDescription "Use first instead of bimap f id"
                              , requiredImports: []
                            }
                        }
                    ]
                else if fIsId && gIsId then
                  [ LintWarning
                      { ruleId: RuleId "UseBimap"
                      , message: WarningMessage "bimap id id is redundant"
                      , range: rangeOf expr
                      , severity: Warning
                      , suggestion: Just $ Suggestion
                          { replacement: ReplacementText "identity"
                          , description: SuggestionDescription "bimap id id is just identity"
                            , requiredImports: []
                          }
                      }
                  ]
                else []
            [ AppTerm fArg, AppTerm gArg, AppTerm xArg ] ->
              let
                fIsId = isIdentity imports fArg
                gIsId = isIdentity imports gArg
                x = printExpr xArg
              in
                if fIsId && not gIsId then
                  let
                    g = printExpr gArg
                  in
                    [ LintWarning
                        { ruleId: RuleId "UseBimap"
                        , message: WarningMessage "bimap id g x can be simplified to second g x"
                        , range: rangeOf expr
                        , severity: Warning
                        , suggestion: Just $ Suggestion
                            { replacement: ReplacementText ("second " <> g <> " " <> x)
                            , description: SuggestionDescription "Use second instead of bimap id"
                              , requiredImports: []
                            }
                        }
                    ]
                else if gIsId && not fIsId then
                  let
                    f = printExpr fArg
                  in
                    [ LintWarning
                        { ruleId: RuleId "UseBimap"
                        , message: WarningMessage "bimap f id x can be simplified to first f x"
                        , range: rangeOf expr
                        , severity: Warning
                        , suggestion: Just $ Suggestion
                            { replacement: ReplacementText ("first " <> f <> " " <> x)
                            , description: SuggestionDescription "Use first instead of bimap f id"
                              , requiredImports: []
                            }
                        }
                    ]
                else if fIsId && gIsId then
                  [ LintWarning
                      { ruleId: RuleId "UseBimap"
                      , message: WarningMessage "bimap id id x is redundant"
                      , range: rangeOf expr
                      , severity: Warning
                      , suggestion: Just $ Suggestion
                          { replacement: ReplacementText x
                          , description: SuggestionDescription "bimap id id x is just x"
                            , requiredImports: []
                          }
                      }
                  ]
                else []
            _ -> []
      -- Match: first f (second g x) or second g (first f x)
      | isFirst imports fn ->
          case NEA.toArray args of
            [ AppTerm fArg, AppTerm innerArg ]
              | ExprApp secondFn secondArgs <- unwrapParens innerArg
              , isSecond imports secondFn
              , [ AppTerm gArg, AppTerm xArg ] <- NEA.toArray secondArgs ->
                  let
                    f = printExpr fArg
                    g = printExpr gArg
                    x = printExpr xArg
                  in
                    [ LintWarning
                        { ruleId: RuleId "UseBimap"
                        , message: WarningMessage "first f (second g x) can be simplified to bimap f g x"
                        , range: rangeOf expr
                        , severity: Hint
                        , suggestion: Just $ Suggestion
                            { replacement: ReplacementText ("bimap " <> f <> " " <> g <> " " <> x)
                            , description: SuggestionDescription "Use bimap instead of first and second"
                              , requiredImports: []
                            }
                        }
                    ]
            _ -> []
      | isSecond imports fn ->
          case NEA.toArray args of
            [ AppTerm gArg, AppTerm innerArg ]
              | ExprApp firstFn firstArgs <- unwrapParens innerArg
              , isFirst imports firstFn
              , [ AppTerm fArg, AppTerm xArg ] <- NEA.toArray firstArgs ->
                  let
                    f = printExpr fArg
                    g = printExpr gArg
                    x = printExpr xArg
                  in
                    [ LintWarning
                        { ruleId: RuleId "UseBimap"
                        , message: WarningMessage "second g (first f x) can be simplified to bimap f g x"
                        , range: rangeOf expr
                        , severity: Hint
                        , suggestion: Just $ Suggestion
                            { replacement: ReplacementText ("bimap " <> f <> " " <> g <> " " <> x)
                            , description: SuggestionDescription "Use bimap instead of first and second"
                              , requiredImports: []
                            }
                        }
                    ]
            _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isBimap :: ImportInfo -> Expr Void -> Boolean
  isBimap imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "bimap" && hasValue imports "bimap"
  isBimap _ _ = false

  isFirst :: ImportInfo -> Expr Void -> Boolean
  isFirst imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "first" && hasValue imports "first"
  isFirst _ _ = false

  isSecond :: ImportInfo -> Expr Void -> Boolean
  isSecond imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "second" && hasValue imports "second"
  isSecond _ _ = false

  isIdentity :: ImportInfo -> Expr Void -> Boolean
  isIdentity imports (ExprIdent (QualifiedName { name: Ident name })) =
    (name == "identity" && hasValue imports "identity") || (name == "id" && hasValue imports "identity")
  isIdentity _ _ = false

