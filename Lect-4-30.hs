module Lect0430 where

import Prelude hiding (filter, map)

--Agenda
    -- Higher order functions (filter, map, foldr, foldl)

data ArithExpr = Plus ArithExpr ArithExpr 
               | Minus ArithExpr ArithExpr
               | Times ArithExpr ArithExpr
               | Leaf Int -- The base case
    deriving Show

evensOnly :: [Int] -> [Int]
evensOnly [] = []
evensOnly (x:xs) = if x `mod` 2 == 0 then x : evensOnly xs else evensOnly xs


fourLettersOnly :: [String] -> [String]
fourLettersOnly [] = []
fourLettersOnly (x:xs) = if length x == 4 then x : fourLettersOnly xs else fourLettersOnly xs
