module Examples.BadCode where

import Prelude
import Data.Traversable (sequenceA)
import Data.Array (concat)

-- UseTraverse: sequenceA (map f x) -> traverse f x
test1 xs = traverse pure xs

-- MapFusion: map f (map g x) -> map (f <<< g) x
test2 xs = map (f <<< g) x

-- MapIdentity: map identity x -> x
test3 xs = xs

-- NotEqual: not (a == b) -> a /= b
test4 a b = a /= b

-- ConcatMap: concat (map f x) -> concatMap f x
test5 xs = concatMap f x

-- UseGuard: if cond then x else pure unit -> when cond x
test6 cond x = when cond action

-- EtaReduce: \x -> f x -> f
test7 = show

-- RedundantBind: x >>= pure -> x
test8 mx = mx
