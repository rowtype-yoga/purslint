module Examples.BadCode where

import Prelude
import Data.Traversable (sequenceA)
import Data.Array (concat)

-- UseTraverse: sequenceA (map f x) -> traverse f x
test1 xs = sequenceA (map pure xs)

-- MapFusion: map f (map g x) -> map (f <<< g) x
test2 xs = map show (map (_ + 1) xs)

-- MapIdentity: map identity x -> x
test3 xs = map identity xs

-- NotEqual: not (a == b) -> a /= b
test4 a b = not (a == b)

-- ConcatMap: concat (map f x) -> concatMap f x
test5 xs = concat (map pure xs)

-- UseGuard: if cond then x else pure unit -> when cond x
test6 cond x = if cond then x else pure unit

-- EtaReduce: \x -> f x -> f
test7 = \x -> show x

-- RedundantBind: x >>= pure -> x
test8 mx = mx >>= pure
