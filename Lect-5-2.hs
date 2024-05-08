{-# LANGUAGE InstanceSigs #-}
-- Makes it possible to write type signatures inside the definition of type class instructions

-- Agenda
    -- Type Classes!


-- Plus 
-- (+) :: Int -> Int -> Int
    -- would only for Ints ie 3+4 

-- (+) :: a -> a -> a
    -- Too general, how would adding bools or strings work ie
        -- True + True
        -- "pika" + "venasaur"
        -- (\x -> x) + (\x -> x) 

-- (+) :: Num a -> a -> a -> a
    -- 'Num a' is a built in type class
    -- Int, Double, etc all implement the Num type class

-- Type Class Definition
-- A set of operations that you can do on values of any type that implements that type class
-- For types that implement Num (eg Int, Double, etc) you can do (+) (-) (*) (abs) etc


--Equal Equal
-- (==) :: a -> a -> Bool
-- Too general
    -- Okay to ask 
        -- 5 == 6
        -- "pika" == "venasaur"
        -- True == True

    -- Not okay to ask
        -- (\x -> x) == (\x -> y)

-- (==) :: Eq a -> a -> a -> Bool
-- For types that implement Eq, you can do == and /= 



data ArithExpr = Plus ArithExpr ArithExpr 
               | Minus ArithExpr ArithExpr
               | Times ArithExpr ArithExpr
               | Leaf Int -- The base case

eval :: ArithExpr -> Int
eval (Leaf n) = n
eval (Plus n m) = eval n + eval m
eval (Minus n m) = eval n - eval m
eval (Times n m) = eval n * eval m

-- `instance` is a Haskell keyword.
-- `instance Eq ArithExpr` means
-- I'm defining an instance of the `Eq` type class
-- for type `ArithExpr`
instance Eq ArithExpr where
    (==) :: ArithExpr -> ArithExpr -> Bool
    (==) n m = eval n == eval m

-- Different ==
    --left is for ArithExprs, right is for return type of eval leaf which is int


-- A "pretty printer" for ArithExprs.
instance Show ArithExpr where
    show :: ArithExpr -> String
    show (Leaf n) = show n
    show (Plus e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Minus e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
    show (Times e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"



-- Sum types are for giving alternatives.
data HouseholdPerson = Lindsey | Alex | Sylvia | Rainbow | Sprinkles
  deriving (Eq, Show)

-- The built-in Ord behavior sucks.
-- Let's do better.
instance Ord HouseholdPerson where
    (<=) :: HouseholdPerson -> HouseholdPerson -> Bool
    (<=) p1 p2 = show p1 <= show p2

    