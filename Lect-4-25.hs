-- Agenda
    -- More user-defined data types (product/sum/recursive types)
        -- Binary tree example
        -- Maybe type
    -- Tail recursion review 
        -- 'where' clause
    
-- Code Walks May 1-7



-- New stuff:
-- A type for binary trees with ints stored at the leaves and internal nodes
data Tree = JustLeaf Int 
          | NodeTree Int Tree Tree
          | NoData
    deriving Show

-- Error without Deriving - "No instance for Show Tree" ie idk how to show this
-- deriving Show tells haskell to figure out how to print it without a user definition (later in course)

exampleTree :: Tree
exampleTree = NodeTree 7 (NodeTree 5 (JustLeaf 1) (JustLeaf 2))
                         (NodeTree 6 (JustLeaf 3) (JustLeaf 4))

-- Practice code that operates on recursively defined data
-- Tree Traversal code:

--                        List [] of Type Int 
preorderTraversal :: Tree -> [Int]
preorderTraversal NoData       = []
preorderTraversal (JustLeaf n) = [n]
--                                              append Left node           append right node
preorderTraversal (NodeTree n left right) = [n] ++ preorderTraversal left ++ preorderTraversal right


inorderTraversal :: Tree -> [Int]
inorderTraversal NoData       = []
inorderTraversal (JustLeaf n) = [n]
--                                         Left node            append node   append right node
inorderTraversal (NodeTree n left right) = inorderTraversal left ++ [n] ++ inorderTraversal right


--Introduce 'Maybe' type - built in
    -- type variable a is just a or nothing, optional variable
    -- data Maybe a =  Just a | Nothing

maybeNumber :: Maybe Int
maybeNumber = Just 5


--Review from Lecture 4-18 --- with Maybe included, Rainbow and Sprinkle with no Pokemon
--                                                           changed from String to (Maybe String)
data NameAgePokemonDatabaseRecord = DBRecord HouseholdPerson Integer (Maybe String)
    deriving Show

--                                                 Added    Added   Blank entry
data HouseholdPerson = Lindsey | Alex | Sylvia | Rainbow | Sprinkles | Shun
    deriving (Show, Eq)
--                                                         Blank entry will also return Nothing

database' :: [NameAgePokemonDatabaseRecord]
database' = [DBRecord Lindsey 42 (Just "bulbasaur"),
             DBRecord Alex 41 (Just "mew"),
             DBRecord Sylvia 6 (Just "houndoom"),
             DBRecord Rainbow 2 Nothing, -- added
             DBRecord Sprinkles 3 Nothing] -- added


lookupAge' :: [NameAgePokemonDatabaseRecord] -> HouseholdPerson -> Integer 
lookupAge' []                    _    = error "That person isn't in the database!"
lookupAge' (DBRecord n a _:recs) name = if n == name then a else lookupAge' recs name
-- Pattern matching on a list 
--      DBRecord n a _  :  first element in list
--     :recs  :  Rest of the list, call it recursively in the else clause^

lookupPokemon :: [NameAgePokemonDatabaseRecord] -> HouseholdPerson -> Maybe String
lookupPokemon [] _ = Nothing
lookupPokemon (DBRecord n _ p:recs) name = if n == name then p else lookupPokemon recs name


-- More Tail Recursion

-- Classic function: 'fact n' should give us the factorial of n
fact :: Int -> Int 
fact n | n <= 1     = 1
fact n | otherwise  = n * fact (n - 1)
    --otherwise matches everything else

-- This operationally behaves
-- fact 5:
-- 5 * fact 4
-- 5 * 4 * fact 3
-- 5 * 4 * 3 * fact 2
-- 5 * 4 * 3 * 2 * fact 1
-- 5 * 4 * 3 * 2 * 1
--120

factTR :: Int -> Int -> Int
factTR n acc | n <=1     = acc
factTR n acc | otherwise = factTR (n-1) (acc * n)

-- This operationally behaves
-- factTR 5 1
-- factTR 4 5   (accumulated 5)
-- factTR 3 20  (accumulated 5*4 = 20)
-- factTR 2 60  (accumulated 5*4*3 = 60)
-- factTR 1 120 (accumulated 5*4*3*2 = 120)
-- 120

-- What can we do about factTR requiring two arguments? Using "where" -- factTR is locked now because of where?

fact' :: Int -> Int
fact' n = helper n 1
    where helper :: Int -> Int -> Int
          helper n acc | n <= 1 = acc
          helper n acc | otherwise = helper (n-1) (acc * n)
