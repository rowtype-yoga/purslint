# Import Checking Plan

## Goal

Make lint rules safer by verifying that identifiers like `map`, `identity`, `<$>` actually refer to the standard Prelude functions, not user-defined functions with the same name.

## Current State

- `Purelint.Imports` module exists with:
  - `getImportInfo :: Module Void -> ImportInfo` - extracts import info from a module
  - `hasValue :: ImportInfo -> String -> Boolean` - check if a value is imported
  - `hasOp :: ImportInfo -> String -> Boolean` - check if an operator is imported
  - Lists of "Prelude-like" modules (Prelude, Data.Functor, Data.Traversable, etc.)
  - Lists of standard values/operators we care about

## Implementation Plan

### Step 1: Update Rule type to receive ImportInfo

```purescript
-- In Purelint/Rule.purs
type RuleContext =
  { imports :: ImportInfo
  }

type Rule =
  { ruleId :: RuleId
  , run :: RuleContext -> Module Void -> Array LintWarning
  }

mkRule :: RuleId -> (RuleContext -> Module Void -> Array LintWarning) -> Rule
```

### Step 2: Update Runner to pass context

```purescript
-- In Purelint/Runner.purs
runRules :: Array Rule -> SourceCode -> Either String LintResult
runRules rules (SourceCode source) = do
  mod <- parseModule source
  let imports = getImportInfo mod
  let ctx = { imports }
  let warnings = rules >>= \rule -> rule.run ctx mod
  pure $ LintResult { warnings, moduleName: getModuleName mod }
```

### Step 3: Update each rule to check imports

Example for FmapId:

```purescript
checkExpr ctx expr = case expr of
  ExprApp fnExpr args ->
    case fnExpr of
      ExprIdent qn | isMapLike qn && hasValue ctx.imports "map" ->
        -- ... existing logic
```

For operators:

```purescript
  ExprOp lhs ops | isIdentity lhs && hasOp ctx.imports "<$>" ->
    -- ... existing logic
```

### Step 4: Handle qualified imports (future)

Track qualified imports like `import Data.Functor as F` and check for `F.map`.

```purescript
type ImportInfo =
  { preludeValues :: Set String
  , preludeOps :: Set String
  , hasPrelude :: Boolean
  , qualifiedModules :: Map String String  -- alias -> module name
  }
```

Then in rules, also check `QualifiedName { module: Just alias }` against the map.

## Rules to Update

All rules that check for specific identifiers:

| Rule | Values to check | Operators to check |
|------|-----------------|-------------------|
| FmapId | `map`, `fmap`, `identity`, `id` | `<$>`, `<#>` |
| MapFusion | `map`, `fmap` | `<$>`, `<#>` |
| UseTraverse | `map`, `fmap`, `sequenceA` | `<$>` |
| ConcatMap | `concat`, `join`, `map`, `fmap` | |
| NotEqual | `not` | `==`, `/=` |
| UseGuard | `pure`, `when`, `unless` | |
| EtaReduce | (any function) | |
| EtaReduceDecl | (any function) | |
| RedundantBind | `pure` | `>>=` |

## Files to Modify

1. `lib/purslint/src/Purelint/Rule.purs` - Add RuleContext
2. `lib/purslint/src/Purelint/Runner.purs` - Pass context to rules
3. `lib/purslint/src/Purelint/Rules/*.purs` - All 9 rule files
4. `bin/purslint-cli/src/Main.purs` - No changes needed (uses Runner)
5. `bin/purslint-lsp/src/LSP.purs` - No changes needed (uses Runner)

## Testing

After implementation, test with:

```purescript
module Test where

-- Should NOT trigger (no Prelude import)
x = map identity [1,2,3]

-- Define our own map
map :: forall a b. (a -> b) -> Array a -> Array b
map f xs = xs
```

vs:

```purescript
module Test where

import Prelude

-- SHOULD trigger (Prelude imported)
x = map identity [1,2,3]
```
