{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Eta reduce" #-}
import Text.XHtml (base)
-- Agenda
    --Midterm Review

    --Part 1
        -- Evaluating LC expressions (lect 2-3, HW0)
        -- Programming in LC (lect 3-5, HW0)
        -- Recursion in LC (Lect 5)
    
    --Part 2
        -- Understanding types of Haskell expressions (lect 5-10, HW1-3)
            -- Including type variables
        -- Pattern matching (lect 5-10, HW1-3)
        -- Tuples (lect 7, HW2-3)
        -- User-defined types (lect 7-10, HW2-3)
            -- Product types, sum types, recursive types
        -- Tail recursion (lect 7-8, HW3)
        -- The 'Maybe' type (lect 8, HW3)
        -- Higher Order Functions (lect 9, )
            -- map, foldr
        -- Type classes
            --defining and using
            -- defining using custom type classes
            -- type class constraints
 
    -- Part 3
        -- **Abstract Syntax Trees and working with them (lect 7-10, HW2-3)
        -- **Environments and working with them (HW3)

--        function       base  list  
foldr' :: (a -> b -> b) -> b -> [a] -> b
-- function, base value, empty list = base value
foldr' f b []     = b
foldr' f b (x:xs) = f x (foldr' f b xs)

-- foldr takes a list of things and boils them down to one thing using a specified function

sumList :: [Int] -> Int
sumList []     = 0
sumList (x:xs) = x + sumList xs

sumList' :: [Int] -> Int
sumList' xs = foldr' (+) 0 xs


-- fold'r (+) 0 [1, 2, 3, 4]
-- (+) 1 (foldr' (+) 0 [2, 3, 4])
-- (+) 1 ((+) 2 (foldr' (+) 0 [3, 4]))
-- etc

