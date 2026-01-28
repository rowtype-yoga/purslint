module Purslint.Imports where

import Prelude

import Data.Array as Array
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)
import Data.Void (Void)
import PureScript.CST.Types (DelimitedNonEmpty(..), Import(..), ImportDecl(..), Module(..), ModuleHeader(..), ModuleName(..), Name(..), Ident(..), Operator(..), Separated(..), Wrapped(..))

-- | Information about what's imported from which module
type ImportInfo =
  { -- Values imported (unqualified) from Prelude-like modules
    preludeValues :: Set String
    -- Operators imported (unqualified) from Prelude-like modules
  , preludeOps :: Set String
    -- Whether Prelude is imported (open or with specific imports)
  , hasPrelude :: Boolean
  }

-- | Check if a module is prelude-like (for debugging)
isPreludeModule :: String -> Boolean
isPreludeModule name = Set.member name preludeModules

-- | Modules considered "Prelude-like" for our purposes
preludeModules :: Set String
preludeModules = Set.fromFoldable
  [ "Prelude"
  , "Data.Functor"
  , "Data.Foldable"
  , "Data.Traversable"
  , "Control.Applicative"
  , "Control.Monad"
  , "Control.Bind"
  , "Control.Alternative"
  , "Data.Function"
  , "Data.Eq"
  , "Data.Boolean"
  , "Data.Maybe"
  , "Data.Tuple"
  , "Data.Monoid"
  , "Data.Semigroup"
  , "Data.Ord"
  , "Data.Array"
  , "Data.Either"
  , "Data.Bifunctor"
  , "Data.List"
  ]

-- | Standard Prelude values we care about
standardValues :: Set String
standardValues = Set.fromFoldable
  [ "map", "identity", "id", "not"
  , "traverse", "traverse_", "sequenceA", "sequence", "for", "for_"
  , "concat", "concatMap", "bind", "pure", "join"
  , "when", "unless"
  , "or", "and", "any", "all"
  , "length", "null", "head", "last", "take", "reverse", "sort"
  , "const", "void"
  , "maybe", "fromMaybe", "isJust", "isNothing", "Nothing", "Just"
  , "flip", "elem", "notElem", "elemIndex", "findIndex"
  , "mempty", "fold", "foldMap", "foldr", "foldl"
  , "fst", "snd", "uncurry"
  , "max", "min", "minimum", "maximum"
  , "compare", "comparing", "on"
  , "guard", "empty"
  , "catMaybes", "mapMaybe", "findMap"
  , "either", "Left", "Right"
  , "bimap", "first", "second"
  , "span", "break"
  , "negate"
  , "zip", "zipWith"
  , "repeat", "replicate"
  ]

-- | Standard Prelude operators we care about
standardOps :: Set String  
standardOps = Set.fromFoldable
  [ "<$>", "<#>", "<*>", ">>=", "=<<", ">>", "*>", "<<<", ">>>", "==", "/="
  , "<>", ">", ">=", "<", "<=", "$>", "!!", "<|>", "&&", "||"
  ]

-- | Extract import information from a module
getImportInfo :: Module Void -> ImportInfo
getImportInfo (Module { header: ModuleHeader header }) =
  let
    imports = header.imports
    results = map analyzeImport imports
  in foldr mergeInfo emptyInfo results

-- | Merge two ImportInfo records
mergeInfo :: ImportInfo -> ImportInfo -> ImportInfo
mergeInfo a b =
  { preludeValues: Set.union a.preludeValues b.preludeValues
  , preludeOps: Set.union a.preludeOps b.preludeOps
  , hasPrelude: a.hasPrelude || b.hasPrelude
  }

-- | Analyze a single import declaration
analyzeImport :: ImportDecl Void -> ImportInfo
analyzeImport (ImportDecl imp) =
  let
    moduleName = case imp.module of
      Name { name: ModuleName n } -> n
    isPreludeLike = Set.member moduleName preludeModules
    isQualified = case imp.qualified of
      Just _ -> true
      Nothing -> false
  in
    if not isPreludeLike || isQualified then
      -- Not a Prelude-like module or it's qualified - ignore
      emptyInfo
    else case imp.names of
      -- Open import: import Prelude
      Nothing ->
        { preludeValues: standardValues
        , preludeOps: standardOps
        , hasPrelude: moduleName == "Prelude"
        }
      -- Selective import: import Prelude (map, (<$>))
      Just (Tuple hiding delimited) ->
        case hiding of
          -- Hiding import - assume everything except hidden is available
          Just _ -> 
            { preludeValues: standardValues
            , preludeOps: standardOps
            , hasPrelude: moduleName == "Prelude"
            }
          -- Explicit imports
          Nothing ->
            let
              imps = getDelimitedItems delimited
              vals = Array.mapMaybe getImportValue imps
              ops = Array.mapMaybe getImportOp imps
            in
              { preludeValues: Set.fromFoldable vals
              , preludeOps: Set.fromFoldable ops
              , hasPrelude: moduleName == "Prelude"
              }

emptyInfo :: ImportInfo
emptyInfo = 
  { preludeValues: Set.empty
  , preludeOps: Set.empty
  , hasPrelude: false
  }

-- | Extract items from a DelimitedNonEmpty
getDelimitedItems :: forall a. DelimitedNonEmpty a -> Array a
getDelimitedItems (Wrapped { value: Separated { head, tail } }) =
  Array.cons head (map snd tail)

-- | Get value name from Import if it's a value import
getImportValue :: Import Void -> Maybe String
getImportValue (ImportValue (Name { name: Ident n })) = Just n
getImportValue _ = Nothing

-- | Get operator name from Import if it's an operator import
getImportOp :: Import Void -> Maybe String
getImportOp (ImportOp (Name { name: Operator n })) = Just n
getImportOp _ = Nothing

-- | Check if a value is available (imported from Prelude-like module)
hasValue :: ImportInfo -> String -> Boolean
hasValue info name = Set.member name info.preludeValues

-- | Check if an operator is available (imported from Prelude-like module)
hasOp :: ImportInfo -> String -> Boolean
hasOp info op = Set.member op info.preludeOps
