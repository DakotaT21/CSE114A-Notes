-- Agenda:
    -- Lists 
        -- List Constructors
        -- Pattern matching on lists
        -- Writing list literals
        -- List append
        -- Strings are also lists
        -- Polymorphic list types
    -- Guards in function definitions
    -- Using 'div' and 'mod'
    -- Using infix functions


-- Lists are either 
    --empty: []
    --have a head *element*, attached to a tail *list*: 

        -- ["pikachu"]
        -- Head "pikachu", tail []

        -- ["pikachu", "mew", "bulbasuar"]
        -- Head "pikachu", tail ["mew", "bulbasuar"]
    
    -- Were always going to have to deal with 
        --empty list case
        --case where list has head and tail

    --Every list is constructed out of either:
        -- empty list constructor []
        -- the : constructor, pronounced "cons" which puts a head and tail together


-- :: means "has type"
-- [a] is a type variable, takes list of int string etc instead of being specific
-- ourLength should return type Int

ourLength :: [a] -> Int
--empty list case
ourLength []     = 0
--head/tail case, head element (not used so _ not x) bound to the tail list, recursive
ourLength (_:xs) = 1 + ourLength xs

--Built-in 'head' function, this is what it looks like
ourHead :: [a] -> a
ourHead []    = error "Empty"
ourHead (x:_) = x


-- Better to use pattern matching instad of 'head' and 'tail', safer, better style
appendHellotoAll :: [String] -> [String]
appendHellotoAll []     = []
appendHellotoAll (x:xs) = (x ++ "hello") : appendHellotoAll xs


-- Strings are just lists of Char
removeFirstChar :: String -> String
removeFirstChar []     = error "Empty"
removeFirstChar (_:xs) = xs
-- This code is redundant with 'tail'


-- :t tells you the type of something
    -- ':t apple' --- "apple" :: String
    -- ':t (++)' --- (+_+) :: [a] -> [a] -> [a]


-- Append
-- can be infix or prefix
    -- (++) "apple" "orange"
    -- "apple" ++ "orange"

-- Function that appends "hello" to only the second element of the list
appendSecond :: [String] -> [String]
-- empty list case
appendSecond []       = []
-- one element list case
appendSecond [x]   = [x]
-- everything else case
appendSecond (x:y:ys) = x : (y ++ "hello") : ys
-- x first elem, y second elem, ys list


-- Guards
lessThanFive :: Int -> String
-- Patten match, | is a guard, after | is a boolean expression evaluating for =
lessThanFive n | n < 5  = "Hooray"
lessThanFive n | n >= 5 ="Boo"

compareNum :: Int -> Int -> String
compareNum n m | n == m = "Hooray"
compareNum n m | n /= m = "Boo"

-- Uses "otherwise" as a catch all, always evaluates to true
compareNum' :: Int -> Int -> String
compareNum' n m | n == m = "Hooray"
                | otherwise = "Boo"


-- Div and Mod
    -- div 4000 10 = 400
    -- 4000 'div' 10 = 400
    -- mod 4321 100 = 21
    -- 4321 'mod' 100 = 21

-- Equals
    -- 5 == 6 = False
    -- (==) 5 6 = False
    -- "apple" == "apple" = True
    -- [1, 2, 3] == [1, 2, 3] = True

    -- Doesnt work on functions
    -- (\x -> x) == (\x -> x) = Error