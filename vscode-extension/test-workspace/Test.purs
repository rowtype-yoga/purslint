module Test where

import Prelude

-- This should trigger a "map identity" warning
x = map identity [1, 2, 3]

-- This should trigger a "redundant if" warning  
y = if true then true else false

-- Clean code - no warnings
z :: Int
z = 42
