message :: String
message = "Welcome to Haskell"


--P takes any type as input ie 5, "orange", whatever, variable name can be anything ie a b c
partiallyAppliedFunction :: p -> p
partiallyAppliedFunction = (\x y -> y) "apple"
--This evaluates to "\y -> y" just the identity function

anotherpartiallyAppliedFunction :: p -> String
anotherpartiallyAppliedFunction = (\x y -> x) "apple"
--This evaluates to "\y -> "apple""


 -- Function types: Types of arguments, type of return, put together in an arrow type
 -- " a-> b"

--If then else in Haskell
favoritePokemon :: Bool -> String
favoritePokemon = \b -> if b then "mew" else "gyarados"

-- Cannot be different types in the same function, wont compile:
-- difTypes :: Bool -> String
-- difTypes = \b -> if b then 5 else "apple"

favoritePokemonMessage :: Bool -> String -> String
favoritePokemonMessage = \b str -> if b then "mew" ++ str else "gyarados" ++ str

partialFavoritePokemonMessage :: String -> String
partialFavoritePokemonMessage = favoritePokemonMessage True

--Normal Definition of a function in Haskell
--You can define with multiple equations and use *pattern matching* to match arguments
idiomaticFavoritePokemonMessage :: Bool -> String -> String
idiomaticFavoritePokemonMessage True str = "mew" ++ str
-- idiomaticFavoritePokemonMessage False str = "gyarados" ++ str
idiomaticFavoritePokemonMessage False " is the best" = "no it isnt"
idiomaticFavoritePokemonMessage False _       = "I agree" 

-- Underscore matches anything
-- Underscore ignored matches anything and is ignored?

ourSum :: Int -> Int
ourSum 0 = 0
ourSum n = n + ourSum (n - 1)

